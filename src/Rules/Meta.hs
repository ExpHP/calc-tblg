#!/usr/bin/env stack
{- stack runghc
   --package=hex
-}
-- NOTE: all other deps of this file are added instead to the deps list of hex,
--       where intero can find them.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}

module Rules.Meta(
    metaRules,
    wrappedMain,
    ) where

import           ExpHPrelude hiding (FilePath, interact)
import qualified "base" Data.List as List
import           "base" System.Environment(lookupEnv)
import           "extra" Control.Monad.Extra(whenM)
import           "shake" Development.Shake.FilePath((</>), normaliseEx)
import           "directory" System.Directory(createDirectoryIfMissing, removePathForcibly, listDirectory, doesPathExist)
import qualified "containers" Data.Set as Set
import           "lens" Control.Lens hiding ((<.>), strict, As)
import qualified "foldl" Control.Foldl as Fold
import qualified "aeson" Data.Aeson as Aeson
import qualified "aeson" Data.Aeson.Types as Aeson
import           "turtle-eggshell" Eggshell hiding (need,view,empty,(</>))
import qualified "terrible-filepath-subst" Text.FilePath.Subst as Subst

-- import qualified Turtle.Please as Turtle hiding (empty)
import           ShakeUtil

wrappedMain :: ShakeOptions -> AppConfig -> App () -> IO ()
wrappedMain shakeCfg appCfg allRules = shakeArgsWith shakeCfg appCfg [] (appFromArgs extendedRules)
  where extendedRules = initApp >> metaRules >> allRules

-------------------------------------------

touchOneMetaruleName :: String
touchOneMetaruleName = "keep"
touchAllMetaruleName :: String
touchAllMetaruleName = "keep-all"

-- FIXME misplaced?
initApp :: App ()
initApp = do
    -- Read rewrite rules written to file.
    whenM (liftIO $ doesPathExist ".rewrites") $ do
        rewrites <- readJSON ".rewrites" :: App [(Pat,Pat)]
        forM_ rewrites $ uncurry registerRewriteRule

metaRules :: App ()
metaRules = do


    -- let the user supply their own pattern.
    -- [p], [v], and [k] will iterate over the values they are
    --   usually expected to have in the patterns used in this file.
    "all:[:**]" ~!> \_ F{..} -> all_NeedVer (fmt "[]")

    -- remove a single file
    "rm:[:**]" ~!> \_ F{..} ->
        liftIO (removePathForcibly (fmt "[]"))

    -------------------
    -- "touch" rules:  'keep:' and 'keep-all:'
    --
    -- These tell shake to accept a file in its current form.
    -- This makes it possible to manually overwrite a file and regenerate its dependencies
    --    based upon the modified content.  (shake tries very hard to prevent this from
    --    happening normally, it seems, giving you only the ability to update immediate
    --    dependencies at best)
    --
    -- It works by simply touching files instead of performing their associated actions.
    --
    -- Due to limitations in implementation, you CANNOT mix touch rules and non-touch rules.
    --
    -- Usage is as follows:
    --
    --       $ # make sure rules run normally first so shake builds the DAG correctly.
    --       $ # Or something like that. I dunno, it helps...
    --       $ ./shake depends-on-a.frob
    --       $
    --       $ # do something to manually modify a file in the dep tree:
    --       $ frobnicate2 a.knob > a.frob
    --       $
    --       $ # tell shake to love this new file like its own child
    --       $ ./shake keep:a.frob
    --       $
    --       $ # Shake will now consider all reverse deps of a.frob out-of-date.
    --       $ # It will regenerate those files when needed, but it will leave a.frob alone.
    --       $ # This works even if there are multiple layers of dependencies between
    --       $ #   the requested file and "a.frob".
    --       $ ./shake depends-on-a.frob
    --
    -- NOTE: these are handled directly from the positional argument stream
    let impossiburu name =
            (name ++ ":[:**]") ~!> \_ _ ->
                fail $ concat ["internal error: '", name, "' should have been handled already"]
    impossiburu touchOneMetaruleName
    impossiburu touchAllMetaruleName

    -------------------
    -- Directory saving
    -- It really sucks debugging rules that "successfully" create incorrect output files,
    --  because properly invalidating these files can be tricky.
    --
    -- Intended usage:
    --
    -- - Encounter a bug which has visibly affected a file tracked by shake.
    -- - Generate a new input directory from scratch
    -- - Request for shake to compute a bunch of files which are known to be correct
    --   (in particular those which take a long time to compute)
    -- - Save
    -- - Debug without fear!
    --
    -- NOTE: ugh.  seems there are cases where this still isn't enough,
    --       presumably due to mixing and matching an old .shake with a new Shakefile?
    --       Tbh I'm not sure what kind of data it stores in there...

    "save:[name]" ~!> \_ F{..} -> do
        let saveDir = namedSaveDir (fmt "[name]")
        liftIO $ removePathForcibly saveDir
        liftIO $ createDirectoryIfMissing True saveDir
        liftIO filesAffectedBySaving
            >>= mapM_ (\s -> cptreeUntracked s (saveDir </> s))

    "restore:[name]" ~!> \_ F{..} -> do
        let saveDir = namedSaveDir (fmt "[name]")
        liftIO $ filesAffectedBySaving >>= mapM_ removePathForcibly
        mergetreeDumbUntracked saveDir "."


data RuleKind
    = PathLike          -- transforms like a path.  (affected by prefixes, etc.)
    | ExactString       -- does not transform like a path.
    | AnyRule           -- takes an argument that is itself a rule (hence we should inspect it recursively)
    | HasArgOf RuleKind -- takes an argument that transforms exactly as specified.
    deriving (Eq, Ord, Show, Read)

-- have fun micromanaging this...
ruleKindMap :: [(String, RuleKind)]
ruleKindMap =
    [ ("all:",      HasArgOf AnyRule)
    , ("rm:",       HasArgOf PathLike)
    , ("keep:",     HasArgOf PathLike)
    , ("keep-all:", HasArgOf PathLike)
    , ("save:",     HasArgOf ExactString)
    , ("restore:",  HasArgOf ExactString)
    ]

namedSaveDir :: FileString -> FileString
namedSaveDir = ("../saves" </>)
filesAffectedBySaving :: IO [FileString]
filesAffectedBySaving = toList . (`Set.difference` blacklist) . Set.fromList <$> listDirectory "."
  where
    blacklist = Set.fromList ["Shakefile.hs", "shake"]

------------------------------------------------------------

-- something is off about the logic in this function but I'm not sure what
transformPathsInArgBy :: (String -> String) -> RuleKind -> String -> String
transformPathsInArgBy _ ExactString arg = arg
transformPathsInArgBy f PathLike    arg = f arg
transformPathsInArgBy _ (HasArgOf _) _ = error "transformPathsInArgBy: case should already be handled" -- XXX this is weird...
transformPathsInArgBy f AnyRule     arg =
    case List.find ((`isPrefixOf` arg) . fst) ruleKindMap of
        Just (prefix, HasArgOf kind) -> prefix ++ transformPathsInArgBy f kind (drop (length prefix) arg)
        Just (_, AnyRule)            -> error "transformPathsInArgBy: AnyRule loop!"
        Just (_, kind)               -> transformPathsInArgBy f kind arg -- XXX awkward case? how did this arise...
        Nothing -> transformPathsInArgBy f PathLike arg -- default assumption

------------------------------------------------------------

-- HACK: tied to input file structure
-- HACK: shouldn't assume ab
allPatterns :: IO [FileString]
allPatterns = getEm >>= dubbaCheck
  where
    getEm = loudIO . fold Fold.list $
        -- HACK: extra parent and /ab/
        (maybe (error "allPatterns: couldn't parse pattern; this is a bug") id
            . List.stripPrefix "input/pat/" . reverse . List.dropWhile (== '/') . reverse) .
        normaliseEx . idgaf . parent . parent <$> glob "input/pat/*/ab/positions.json"
    dubbaCheck [] = fail "allPatterns: found no patterns; this is probably a bug"
    dubbaCheck x | any ('/' `elem`) x = fail "allPatterns: produced an invalid pattern; this is a bug"
    dubbaCheck x = pure x

sortOnM :: (Monad m, Ord b)=> (a -> m b) -> [a] -> m [a]
sortOnM f xs = do
    keys <- mapM f xs
    pure $ map fst $ List.sortBy (comparing snd) $ zip xs keys

-- HACK: tied to input file structure
-- HACK: shouldn't assume ab
patternVolume :: FileString -> Act Int
patternVolume p = do
    json <- needs $ "input/pat" </> p </> "ab/positions.json"
    result <- maybe undefined id <$> readJSON json
    pure . maybe undefined id . flip Aeson.parseMaybe result $
        (Aeson..: "meta") >=> (Aeson..: "volume") >=> (aesonIndex 0)

---------------------------------
-- argument parsing for touch rules

appFromArgs :: App () -> [()] -> [String] -> IO (Maybe (App ()))
appFromArgs allRules _ args' = do

    prefix <- liftIO $ maybe "" id <$> lookupEnv "SHAKE_WANT_PREFIX"
    let args = transformPathsInArgBy (prefix </>) AnyRule <$> args'

    let touchAppAssoc :: [(String, String -> App ())]
        touchAppAssoc = [ (touchOneMetaruleName, \p -> want [p])
                        , (touchAllMetaruleName, all_TouchVer)
                        ]

    let touchApp :: String -> Maybe (App ())
        touchApp arg = foldl (<|>) empty
                     $ fmap (\(name, cb) -> fmap cb (List.stripPrefix (name ++ ":") arg)
                            ) touchAppAssoc
    let isTouchArg = isJust . touchApp

    let hasTouchArg = any isTouchArg args
    let hasNonTouchArg = any (not . isTouchArg) args

    case (hasTouchArg, hasNonTouchArg) of
        -- NOTE: current implementation cannot support a mixture of non-touch and touch rules.
        --       Note that any implementation that tries to fix this will need to somehow
        --          cope with the fact that shake runs rules in an arbitrary order.
        (True, True) -> fail $ concat [ "cannot mix touch args with non-touch args" ]

        -- all plain args.  Run normally.
        (False, True)  -> pure $ Just $ allRules >> want args

        -- all touch args.  Convert each to an App and run them.
        (True, False) -> pure $ Just $ withTouchMode allRules >>
            case mapM touchApp args of Just apps -> foldl (>>) (pure ()) apps
                                       Nothing   -> fail "appFromArgs: internal error"

        (False, False) -> pure Nothing

---------------------------------

-- TODO refactor

all_NeedVer :: Pat -> Act ()
all_NeedVer pat = do
    ps <- liftIO allPatterns >>= sortOnM patternVolume
    vs <- pure ["vdw", "novdw"]
    as <- pure ["aba", "abc"]

    let mapMaybe f xs = f <$> xs >>= maybe [] pure
    let Identity func = (iDontCare . Subst.compile) "[p]:::[v]:::[a]"
                        >>= (iDontCare . flip Subst.substIntoFunc pat)

    let pvks = List.intercalate ":::" <$> sequence [ps,vs,as]
    -- Shake will do the files in arbitrary order if we need them all
    -- at once which sucks because it is nice to do lowest volume first.
    -- need $ orderPreservingUnique (mapMaybe func pvks)
    mapM_ needs $ orderPreservingUnique (mapMaybe func pvks)

all_TouchVer :: Pat -> App ()
all_TouchVer pat = do
    ps <- liftIO allPatterns
    vs <- pure ["vdw", "novdw"]
    as <- pure ["aba", "abc"]

    let mapMaybe f xs = f <$> xs >>= maybe [] pure
    let Identity func = (iDontCare . Subst.compile) "[p]:::[v]:::[a]"
                        >>= (iDontCare . flip Subst.substIntoFunc pat)

    let pvks = List.intercalate ":::" <$> sequence [ps,vs,as]
    want $ orderPreservingUnique (mapMaybe func pvks)

---------------------------------

iDontCare :: (Monad m)=> Either String a -> m a
iDontCare = either fail pure -- https://www.youtube.com/watch?v=ZXsQAXx_ao0

-- get unique elements ordered by first occurrence
orderPreservingUnique :: (_)=> [a] -> [a]
orderPreservingUnique xs = f (Set.fromList xs) xs
  where
    f set _  | null set = []
    f _   []            = []
    f set (x:xs) | x `Set.member` set = x : f (Set.delete x set) xs
                 | otherwise          = f set xs

-- | @cp -a src/* dest@.  "Dumb" because it will fail if the trees have any
--   similar substructure (i.e. src\/subdir and dest\/subdir)
mergetreeDumbUntracked :: (_)=> FileString -> FileString -> Act ()
mergetreeDumbUntracked src dest =
    liftIO (listDirectory src) >>=
        mapM_ (\entry -> cptreeUntracked (src </> entry) (dest </> entry))

-- | @cp -a src dest@
cptreeUntracked :: (_)=> FileString -> FileString -> Act ()
cptreeUntracked src dest = liftAction $ cmd "cp" ["-a", src, dest] (Traced "")
