{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module ShakeUtil.Defs where

import           Prelude hiding (FilePath, interact)
import           "base" Data.String(IsString(..))
import           "base" Control.Monad
import           "base" Debug.Trace
import qualified "base" Data.List as List
import           "mtl" Control.Monad.Identity
import           "transformers" Control.Monad.Trans.Reader(ReaderT(..))
import           "mtl" Control.Monad.Reader
import qualified "shake" Development.Shake as Shake
import           "mtl" Control.Monad.Trans(MonadTrans(..))
import           "filepath" System.FilePath.Posix((</>))
import           "directory" System.Directory
import           "shake" Development.Shake.FilePath(normaliseEx)
import qualified "terrible-filepath-subst" Text.FilePath.Subst as Subst
import           ShakeUtil.Wrapper
import           ShakeUtil.Types

-------------------------------------------------------------------
--

actToAction :: ActGlobal -> Act a -> Action a
actToAction cfg = flip runReaderT cfg . runAct

appToRules :: AppGlobal -> App a -> Rules a
appToRules cfg = flip runReaderT cfg . runApp

-- A modified (*>) inspired by the one here:
--        https://github.com/ndmitchell/shake/issues/485#issuecomment-247858936
--
-- However, rather than exposing the components of matches, it exposes a
-- "format function" which takes substitution templates and substitutes the
-- matched content into them.
--
-- The match syntax is not Shake's or regex, but something else entirely.
-- "[]" (in match strings or replacement templates) is an unnamed capture.
-- "[name]" (in match strings or replacement templates) is a named capture.
-- By default, matched parts are restricted to one component, like a glob ("*").
-- "[:**]" or "[name:**]" in a match string indicates that it can match multiple
-- components, like a globstar ("**").
--
-- So for instance,  @"[a]/in/[b].tex" !> \path fmt@ would match @"abc/in/fig.tex"@,
-- and @fmt "[a]/out/[b].pdf"@ would return @"abc/out/fig.pdf"@.
(!>) :: Pat -> ActFun -> App ()
(!>) = makeRule shakeRules

-- Make a phony command that operates on subst patterns.
-- This can be used to supply phony commands with arguments.
(~!>) :: Pat -> ActFun -> App ()
(~!>) = makeRule shakePhonys

shakeRules :: (FileString -> Maybe (Action ())) -> Rules ()
shakeRules = maybeificate (Shake.?>)

shakePhonys :: (FileString -> Maybe (Action ())) -> Rules ()
shakePhonys = Shake.phonys

singItBrutha :: (Monad m)=> Either String a -> m a
singItBrutha = either fail pure

-- shared logic for various rule constructors
makeRule :: ((FileString -> Maybe (Action ())) -> Rules ())
         -> Pat -> ActFun -> App ()
makeRule mkShakeRule patSuffix act = do

    -- compile a composite pattern built from the prefix and the user's pattern
    pat <- autoPrefix patSuffix
    subst <- (singItBrutha . Subst.compile) pat

    extractPrefix <- (singItBrutha . Subst.substIntoFunc subst) =<< askPrefix

    -- Create the rule for Shake
    liftRules . mkShakeRule $ \path -> do -- :: Maybe _

        -- Give the ActFun the *full* path as well as a formatting function
        --  for the *full* pattern.
        fmtM <- traceShow ("subst", pat, "path", path) Subst.substAgainstFunc subst path
        let Just matchedPrefix = extractPrefix path -- infallible

        let fmt_ :: Fmt -- type inference is taking a day off here...
            fmt_ = fromString . runIdentity . singItBrutha . fmtM
            file_ :: Fmt
            file_ = fromString .  (\s -> trace ("subst " ++ show pat ++ " " ++ show s ++ " " ++ show path ++ " = " ++ fmt_ s) $ (matchedPrefix </>) $ fmt_ s)
            fmts = F {fmt=fmt_, file=file_}

        let global = ActGlobal fmts
        Just $ actToAction global $ act path fmts

trace1 :: (Show a, Show b)=> String -> (a -> b) -> (a -> b)
trace1 msg f a = trace (msg ++ " " ++ show a ++ " = " ++ show (f a)) (f a)
{-
-- What follows resembles the sort of implementation that might
-- be necessary to truly abstract the prefix away, allowing code
-- inside an "enter" block to be written as though it belonged to
-- a Shakefile that existed within the matched directory.
--
-- The kicker is dependencies; it would be too much work to wrap all
-- of the various functions that can potentially produce a dependency
-- in order to ensure that shake receives the correct paths relative
-- to the shake root.

    -- compile a substitution mechanism for cfgPrefix
    let suffixWild = "awkwardlyNamedInternalUseSuffix"
    let suffixWildSub = "[" ++ suffixWild ++ ":**]"
    let suffixWildRep = "[" ++ suffixWild ++ "]"
    prefix <- askPrefix
    subst <- autoPrefix suffixWildSub
    subst <- Subst.compile subst

    -- matchers for the full pattern
    extractPrefix <- Subst.substIntoFunc subst prefix
    extractSuffix <- Subst.substIntoFunc subst suffixWildRep
    let splitMatch :: String -> Maybe (String, String)
        splitMatch s = (,) <$> extractPrefix s <*> extractPrefix s

    -- this runs after, to match the user's pattern
    innerSubst <- Subst.compile pat

    -- Create the rule for Shake
    let rules :: Rules ()
        rules = mkShakeRule $ \path -> do -- :: Maybe _

            -- perform the actual matching (these are both Maybe)
            (matchedDir, matchedSuff) <- splitMatch path
            eitherFmt <- Subst.substAgainstFunc innerSubst matchedSuff
            let fmt = Fmt $ fromString . either fail id . eitherFmt

            --
            Just $ do -- :: Action _
                liftIO $ createDirectoryIfMissing True matchedDir
                id $ actInDir matchedDir
                $ actToAction matchedDir
                $ act matchedSuff fmt

    liftRules rules
-}

---------------------------------------


surrogateRoot :: FileString -> FileString
surrogateRoot = normaliseEx . (++ "/../..")

-- | Associate multiple outputs with a single action, in a manner
--   tied to directory structure.
--
--   The (unfortunate) syntax:
--
--   >    surrogate "make-inputs"
--   >        [ ("[]/spatial-params.toml", 2)
--   >        , ("[]/positions.json", 2)
--   >        , ("[]/supercells.json", 2)
--   >        , ("shake", 1)
--   >        , ("Shakefile.hs", 1)
--   >        ] $ "data/[:**]" #> \root fmt -> do
--   >            doSomeAction ...
--
--   creates a rule for an empty marker file at "data\/[]\/.surrogate\/make-inputs"
--   which performs the action, supplying the match to "data\/[]" in 'root'.
--
--   The patterns are interpreted relative to 'root', and the action is expected
--   to create ALL instances of files matching the given patterns within 'root'
--   in a single run.
--
--   "name" is not something that will ever necessarily need to be referred to again;
--   but it should be chosen to be both unique across all surrogates (at least,
--   those that may match a given directory), and persistent across runs.
--
-- NOTE: due to lousy implementation it also currently requires you to
--       specify how many components each path contains/is expected to match.
--       This means no variable-depth wildcards in the output file patterns!
surrogate :: FileString -> [(FileString, Int)] -> SurrogateSpec -> App ()
surrogate surrogateName filesMade (SurrogateSpec pat userFunc) = do

    -- produce surrogate file along with running actual rule
    let surrogatePat = pat </> ".surrogate" </> surrogateName
    surrogatePat !> \sfile fmt ->
        -- give the user function the path THEIR regex matched, not ours
        let root = normaliseEx (sfile </> "../..")
        in userFunc root fmt
           >> liftIO (writeFile sfile "")

    -- make created files "depend" on the surrogate
    forM_ filesMade $ \(name,compCount) ->
        (pat </> name) !> \f _ ->
            -- hrrng, now we must recover the surrogate file from the path
            let root = normaliseEx $ foldl (</>) f (replicate compCount "..") in
            needSurrogate surrogateName root

-- TODO: surrogateFile which does not require a Pat
surrogateFile :: FileString -> [(FileString, Int)] -> SurrogateSpec -> App ()
surrogateFile = error "TODO"

-- Part of the syntax of 'surrogate'.
data SurrogateSpec = SurrogateSpec Pat ActFun
(#>) :: Pat -> ActFun -> SurrogateSpec
(#>) = SurrogateSpec

needSurrogate :: FileString -> FileString -> Act ()
needSurrogate = _needSurrogateImpl need

needSurrogateFile :: FileString -> FileString -> Act ()
needSurrogateFile = _needSurrogateImpl needFile

neededSurrogate :: FileString -> FileString -> Act ()
neededSurrogate = _needSurrogateImpl needed

_needSurrogateImpl :: ([FileString] -> Act ())
                   -> FileString -> FileString -> Act ()
_needSurrogateImpl need name root =
    let sfile = root </> ".surrogate" </> name
    in need [sfile]

predificate :: ((a -> Maybe b) -> c) -> ((a -> Bool) -> (a -> b) -> c)
predificate f b v = f (matchPair . makePair)
  where
    matchPair (True, x) = Just x
    matchPair (False,_) = Nothing
    makePair = (,) <$> b <*> v  -- a -> (Bool, b)

maybeificate :: ((a -> Bool) -> (a -> b) -> c) -> ((a -> Maybe b) -> c)
maybeificate f g = f (isJust . g) (fromJust . g)
  where
    isJust   = maybe False (const True)
    fromJust = maybe (error "maybeificate: tried to use missing value") id

-- pat ~!> act = phonys $ \path ->
--         case Subst.substAgainstFunc subst path of
--             Nothing -> Nothing
--             Just f  -> Just . wrapTraceIO ("'" ++ pat ++ "' on '" ++ path)
--                        $ act path (Fmt $ fromString . either fail id . f)
--     where
--         Identity subst = Subst.compile pat

-- | Defines a family of files created by a single rule.
--
--   syntax:
--
--   >  family
--   >      [ "[p]/atter/[n]/test.txt"
--   >      , "[p]/atter/[n]/thing.bib"
--   >      , "[p]/[n]/out/output.pdf"
--   >      ] &!> \[testTxt, thingBib, outPdf] fmt -> do
--   >          someActionHere
--
--   The generated rule matches any of the input patterns; All patterns must share
--   the same named captures and number of positional captures.
--   When the action runs, it is expected to produce one file matching each pattern;
--    these are provided in a list.
--
-- (this solves a different use-case from 'surrogate', which is capable of supporting
--  patterns with varying numbers of captures, but is tied to directory structure)
--
-- NOTE: You can omit 'family'; all the magic is in the operator.
--       'family' is just there to give an excuse for the indentation required
--         by do-block notation.
--
-- NOTE: glob specs are not properly supported in these patterns.
--       (every capture will have to be a standard, single-component glob)
family :: [Pat] -> [Pat]
family = id -- well, I'M POOPED.

-- | Part of the syntax for 'family.'
(&!>) :: [Pat] -> ([FileString] -> Fmts -> Act ()) -> App ()
pats &!> act = do
    mapM_ id $ List.zipWith (isSideProductOfFile) pats (drop 1 pats)
    last pats !> \_ fmts@F{..} -> act (file <$> pats) fmts

wrapTraceIO :: (_)=> String -> io a -> io a
wrapTraceIO s io = do
    liftIO . traceIO $ "Entering: " ++ s
    x <- io
    liftIO . traceIO $ "Leaving: " ++ s
    pure x

-- Wrapper around (~!>) providing a straight forward-implementation
--  of a phony rule that takes an argument.
--
-- @"rulename" ~:> \s -> ...@ creates a phony rule invoked as "rulename:ARGS",
-- which will receive "ARGS" in @s@.  (it will also detect accidental usage
--  of "rulename" without an argument)

-- (~:>) :: String -> (String -> Action ()) -> Rules ()
-- (~:>) = error "TODO"

fail' :: (Monad m)=> [Char] -> [Char] -> m a
fail' pre = fail . (pre ++)
traceShowThru :: (Show a)=> (b -> a) -> b -> b
traceShowThru f x = traceShow (f x) x

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

-- We're abusing this option for science.
dontTrack :: Action a -> Action a
dontTrack = orderOnlyAction

-- Declare that the LHS is simply copied from the RHS.
-- (the RHS will be tracked for changes)
isCopiedFromFile :: Pat -> Pat -> App ()
isCopiedFromFile opat ipat = opat !> \path F{..} -> copyFileChanged (file ipat) path

-- Declare the RHS already exists. Like an axiom, of sorts.
thereExistsFile :: MathematicalIntroduction -> Pat -> App ()
thereExistsFile Suppose pat = pat !> \_ _ -> pure ()

data MathematicalIntroduction = Suppose deriving (Eq, Show)

-- Declare that one file is always produced alongside another.
-- (this makes the LHS simply depend on the RHS)
isSideProductOfFile :: Pat -> Pat -> App ()
isSideProductOfFile opat ipat = opat !> \_ F{..} -> need [file ipat]

---------------------------------------

-- Depend on paths, then return them for the purpose of name binding.
needs :: (MonadAction action, IsString string)=> FileString -> action string
needs path = need [path] >> pure (fromString path)

askFile :: Pat -> Act FileString
askFile s = asks $ \ActGlobal{actFmts=F{file}} -> file s

needsFile :: Pat -> Act FileString
needsFile s = askFile s >>= needs

needFile :: [Pat] -> Act ()
needFile s = mapM askFile s >>= need

neededFile :: [Pat] -> Act ()
neededFile s = mapM askFile s >>= needed

shakeArgs :: ShakeOptions -> App () -> IO ()
shakeArgs opts app = Shake.shakeArgs opts $ appToRules config app
  where
    config = AppGlobal
        { appPrefix = ""
        }

class (Functor m)=> Prefix m where
    askPrefix :: m String

    -- note: </> does correctly account for when prefix is ""
    autoPrefix :: String -> m String
    autoPrefix path = fmap (</> path) askPrefix

instance Prefix App where askPrefix = asks appPrefix
instance Prefix Act where askPrefix = asks actPrefix

enter :: Pat -> App a -> App a
enter pat app = do
    newPrefix <- autoPrefix pat
    let f = runReaderT . runApp $ app in
        App . ReaderT $ \cfg -> f cfg{appPrefix=newPrefix}
