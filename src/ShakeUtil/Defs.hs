{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module ShakeUtil.Defs where

import           Prelude hiding (FilePath, interact)
import           "base" Data.String(IsString(..))
import           "base" Control.Monad
import           "base" Debug.Trace
import           "base" Control.Exception
import qualified "base" Data.List as List
import           "extra" System.IO.Extra(newTempDir,newTempFile)
import           "extra" Control.Monad.Extra(unlessM,whenM)
import           "mtl" Control.Monad.Identity
import           "transformers" Control.Monad.Trans.Reader(ReaderT(..))
import           "mtl" Control.Monad.RWS
import           "mtl" Control.Monad.Reader
import           "mtl" Control.Monad.State
import qualified "shake" Development.Shake as Shake
import           "shake" Development.Shake.FilePath(normaliseEx, (</>), splitDirectories, takeDirectory, takeFileName, joinPath)
import qualified "shake" Development.Shake.Command as Shake
import           "directory" System.Directory(createDirectoryIfMissing, canonicalizePath, doesPathExist, pathIsSymbolicLink, removePathForcibly)
import qualified "terrible-filepath-subst" Text.FilePath.Subst as Subst
import           ShakeUtil.Wrapper
import           ShakeUtil.Types
import           UnixUtil(forciblyLinkOrCopy)

-------------------------------------------------------------------
--

-- Uh... basically just runReaderT.
actToAction :: ActGlobal -> Act a -> Action a
actToAction cfg = flip runReaderT cfg . runAct

-- Uh... basically just runRWST.
appToRules :: AppGlobal -> AppState -> App a -> Rules (a, AppState)
appToRules cfg initState app = do
    -- drop the modified state and writer junk
    (a, s, ()) <- runRWST (runApp app) cfg initState
    pure (a,s)

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

-- Make it an error to `need` a certain file which is frequently modified.
-- Of course, shake will already do that assuming you don't accidentally add a rule for it;
--   so this rule is here to confict with the rule you might accidentally add.
-- It will also catch `need`ing the file when it happens to already exist.
ephemeralFile :: Pat -> App ()
ephemeralFile pat = pat !> \f _ -> fail (f ++ ": is ephemeral and it is an error to `need` it!")

-- shared logic for various rule constructors
makeRule :: ((FileString -> Maybe (Action ())) -> Rules ())
         -> Pat -> ActFun -> App ()
makeRule mkShakeRule patSuffix actFun = do

    -- Absolute first step is to attach the prefix, to ensure that
    --   ("a/b" !> ...) and (enter "a" $ "b" !> ...) are always equivalent.
    -- Rewrite rules are applied shortly thereafter, to help minimize the difference
    --   between a rule for a symlinked file and a rule for its target.
    pat' <- autoPrefix patSuffix
    pat <- applyRewriteRules pat'
    whenM (asks appDebugRewrite) $ liftIO . putStrLn . concat $
        if pat' == pat then ["# NOT REWRITTEN: ", pat]
                       else ["#     REWRITTEN: ", pat', "\n#               ~~> ", pat]

    subst <- (singItBrutha . Subst.compile) pat

    extractPrefix <- (singItBrutha . Subst.substIntoFunc subst) =<< askPrefix

    -- get this before entering Maybe...
    debugMatches <- asks appDebugMatches

    -- Create the rule for Shake
    liftRules . mkShakeRule $ \path -> do -- :: Maybe _

        -- Give the ActFun the *full* path as well as a formatting function
        --  for the *full* pattern.
        fmtM <- Subst.substAgainstFunc subst path -- & traceShow ("subst", pat, "path", path)
        let Just matchedPrefix = extractPrefix path -- infallible

        let fmt_ :: Fmt -- type inference is taking a day off here...
            fmt_ = fromString . runIdentity . singItBrutha . fmtM
            file_ :: Fmt
            file_ = fromString .  (\s -> tracer s $ (matchedPrefix </>) $ fmt_ s)
            fmts = F {fmt=fmt_, file=file_}

            --tracer s = trace ("subst " ++ show pat ++ " " ++ show s ++ " " ++ show path ++ " = " ++ fmt_ s)
            tracer _ = id

        let global = ActGlobal fmts

        -- display matches when requested, to help debug "multiple matching rules"
        let !() = if debugMatches then traceShow (path, pat) () else ()
        Just $ actToAction global $ actFun path fmts

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

needSurrogate :: String -> FileString -> Act ()
needSurrogate = _needSurrogateImpl need

needSurrogateFile :: String -> FileString -> Act ()
needSurrogateFile = _needSurrogateImpl needFile

neededSurrogate :: String -> FileString -> Act ()
neededSurrogate = _needSurrogateImpl needed

_needSurrogateImpl :: ([FileString] -> Act ())
                   -> String -> FileString -> Act ()
_needSurrogateImpl needIt name root =
    let sfile = root </> ".surrogate" </> name
    in needIt [sfile]

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
pats &!> actF = do
    mapM_ id $ List.zipWith (isSideProductOfFile) pats (drop 1 pats)
    last pats !> \_ fmts@F{..} -> actF (file <$> pats) fmts

wrapTraceIO :: (MonadIO io)=> String -> io a -> io a
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

-- (goes together with copyPath from ShakeUtil.Wrapper)
-- Hard link a path, tracking the source as a dependency.  (unix only)
linkPath :: (MonadAction act)=> FileString -> FileString -> act ()
linkPath s d = liftAction $ Shake.need [s] >> liftIO (forciblyLinkOrCopy s d)

-- (goes with readLines, writeLines from ShakeUtil.Wrapper)
-- (beware the possibility of non-conformant text files which lack a terminating
--  newline; this function may in the future automatically correct such files)
appendLines :: (MonadAction act) => FileString -> [String] -> act ()
appendLines fp = liftAction . liftIO . appendFile fp . unlines

-- Declare that the LHS is simply copied from the RHS.
-- (the RHS will be tracked for changes)
isCopiedFromFile :: Pat -> Pat -> App ()
isCopiedFromFile opat ipat = opat !> \path F{..} -> copyChanged (file ipat) path
-- Like `isCopiedFromFile` but using a hard link, for big files.
isHardLinkToFile :: Pat -> Pat -> App ()
isHardLinkToFile opat ipat = opat !> \path F{..} -> do
    src <- needsFile ipat
    linkPath src path

-- Shorthand for `isCopiedFromFile`/`isHardLinkToFile` when the source and destination filename match.
--
-- These are useful when drafting new stages of computation, but once the dust settles it is recommended
--  to expand these back to `xyzFromFile` declarations, since these obfuscate the answer to common questions
--  like "where is (some file) used?"
isCopiedFromDir :: Pat -> Pat -> App ()
isLinkedFromDir :: Pat -> Pat -> App ()
(isCopiedFromDir, isLinkedFromDir) = (impl isCopiedFromFile, impl isHardLinkToFile)
  where
    impl funcForFile opat ipat =
        let namepat = takeFileName opat in
        opat `funcForFile` (ipat </> namepat)

-- Declare the argument already exists. Like an axiom, of sorts.
thereExistsFile :: Pat -> App ()
thereExistsFile pat = pat !> \_ _ -> pure ()

-- Declare that one file is always produced alongside another.
-- (this makes the LHS simply depend on the RHS)
isSideProductOfFile :: Pat -> Pat -> App ()
isSideProductOfFile opat ipat = opat !> \_ F{..} -> need [file ipat]

---------------------------------------

-- adapted from Shake's functions

withTempFile :: (FileString -> Act a) -> Act a
withTempFile actFun = do
    (dir,del) <- liftIO newTempFile
    action0 <- dipIntoAction (actFun dir)
    liftAction $ action0 `Shake.actionFinally` del

withTempDir :: (FileString -> Act a) -> Act a
withTempDir actFun = do
    (dir,del) <- liftIO newTempDir
    action0 <- dipIntoAction (actFun dir)
    liftAction $ action0 `Shake.actionFinally` del

-- a version of withTempDir that keeps the directory around after failure
withTempDirDebug :: (FileString -> Act a) -> Act a
withTempDirDebug actFun = do
    (dir,del) <- liftIO newTempDir
    actionOnException
        (putStrLn $ "withTempDirDebug: Not deleting due to error: " ++ dir)
        (actFun dir <* liftIO del)

---------------------------------------

-- Declare that src is a symlink to target.
--
-- Effort is made to let this work transparently with shakefile rules;
--  - Files can be 'need'ed from 'src'; this will automatically need the files from 'target'.
--  - Any rules after the `isDirectorySymlinkTo` rule can produce paths in 'src'; these
--     rules will be rewritten to produce paths in 'target' before they are given to Shake.
--
-- The purpose of this is to help facilitate working with "componentized" computations
-- that have dedicated subtrees.  For instance, suppose there was a dedicated directory
-- for frobnicating knobs.  With the help of symlinks, other parts of the shakefile can
-- delegate all of their frobnication to this directory without leaving their 'enter' block:
--
--     -- a componentized rule with a dedicated subtree.
--     -- [owner] and [namespace] exist to help disambiguate between places that use the rule.
--     "frobnicate/[owner]/[namespace]/[name].frob" !> frob F{..} -> do
--         knob <- needsFile "frobnicate/[owner]/[namespace]/[name].knob"
--         let frobData = frobnicate knob
--         writePath frob frobData
--
--     -- elsewhere
--     "abc/frobnicate" `isDirectorySymlinkTo` "frobnicate/abc/all"
--     enter "abc" $ do
--         -- make some knobs
--         "a.knob" !> ...
--         "b.knob" !> ...
--
--         -- We can frobnicate them without needing to leave the 'enter' prefix
--         --  and without having to constantly remember noisy details like 'abc/all'.
--         "frobnicate/[x].knob" `isCopiedFromFile` "[x].knob"
--         "[x].frob" `isCopiedFromFile` "frobnicate/[x].frob"
--
-- CAVEATS:
--  - The symlink rule must come before rules that use the symlink in the LHS.
--  - Don't nest symlinks.  They will be rewritten in an arbitrary order and may fail to resolve.
--  - Rules are rewritten by performing substitutions *directly on the pattern* in the rule's LHS.
--    This should have predictable behavior so long you stick to simple, named, non-globstar matchers.
--
------------------------------------------
-- NOTE: its tempting to think that a simpler solution to this use case would
--       be to not use symlinks, and to instead have a `Pat -> Pat -> App Fmts` function that
--       produces formatters into the targeted dir.
--
--       This would fix the issue of order-dependence by statically preventing bad programs;
--        but it would be far tougher to use IMO since the formatters would generally only
--        be aware of a subset of your capture groups.
isDirectorySymlinkTo :: Pat -> Pat -> App ()
isDirectorySymlinkTo link target = do

    -- Set up a blanket rule that takes any requests for files behind the link and
    --  forwards them to matching files in the target directory.
    (link </> "[symlinkDependWild:**]") !> \_ F{..} -> do
        -- ensure the symlink is there
        -- If this is our first time asking for a file inside this link, then Shake will have kindly
        --  constructed an entire prefix of REAL directories for us.  Tear them out to make room.
        liftIO $ unlessM (pathIsSymbolicLink (file link))
            (removePathForcibly (file link))

        liftAction $ unlessM (liftIO $ doesPathExist (file link)) $ do
            relPath <- liftIO $ makeRelativeEx (file link) (file target)
            cmd "ln" ["-s", relPath, file link]

        () <$ needFile [target </> "[symlinkDependWild]"]

    -- Rewrite all future recipes for the linked directory to use the target instead
    prefix <- asks appPrefix
    registerRewriteRule (prefix </>   link </> "[symlinkRewriteWild:**]")
                        (prefix </> target </> "[symlinkRewriteWild]")


-- Like makeRelative, except it isn't afraid to add ".."
makeRelativeEx :: FileString -> FileString -> IO FileString
makeRelativeEx x' y' = do
    x <- splitDirectories <$> canonicalizePath (takeDirectory x')
    y <- splitDirectories <$> canonicalizePath y'
    return $ joinPath $ if take 1 x /= take 1 y then y else f x y
    where
        f (x:xs) (y:ys)
            | x == y = f xs ys
            | otherwise = ".." : f xs (y:ys)
        f _ ys = ys

---------------------------------------
-- internal functions for rewrite rules

registerRewriteRule :: Pat -> Pat -> App ()
registerRewriteRule from to = do
    s <- get
    let s' = s {appRewriteRules=(from,to):appRewriteRules s}
    put s'

applyRewriteRules :: Pat -> App Pat
applyRewriteRules pat = do
    rules <- gets appRewriteRules

    -- apply a rule if it matches, else return the input unchanged
    let subst' :: String -> String -> String -> App String
        subst' from to s = fmap (maybe s id)
                         . either error pure
                         $ Subst.subst from to $ s

    foldM (\s (from,to) -> subst' from to s) pat rules

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
shakeArgs opts app = shakeArgs' opts appDefaultConfig app

shakeArgs' :: ShakeOptions -> AppGlobal -> App () -> IO ()
shakeArgs' opts config app = Shake.shakeArgs opts (() <$ appToRules config st app)
  where
    st = AppState
        { appRewriteRules = []
        }

class (Functor m)=> Prefix m where
    askPrefix :: m String

    -- note: </> does correctly account for when prefix is ""
    autoPrefix :: String -> m String
    autoPrefix path = fmap (</> path) askPrefix

instance Prefix App where askPrefix = asks appPrefix
instance Prefix Act where askPrefix = asks actPrefix

-- perform an Act now, inside an App.
-- prefixes will be ignored because there is no path to match against.
act :: Act a -> App ()
act = liftRules . action . flip runReaderT cfg . runAct
  where
    cfg = ActGlobal { actFmts = F (error "no fmt in act") (error "no fmt in act")
                    }

enter :: Pat -> App a -> App a
enter pat app = do
    newPrefix <- autoPrefix pat
    local (\x -> x{appPrefix=newPrefix}) app

----------------------------------------------------

-- | Polymorphic type of a Shake "cmd".
--   This is a helper type alias for annotating functions that return commands,
--   so that they do not succumb to the monomorphism restriction.
type PartialCmd = forall arg. (Shake.CmdArguments arg)=> arg

----------------------------------------------------

-- | A meaningless form of specification whose only purpose is to
--   be read by a human looking at the source code.
--
-- Like comments, it can go out of date, but this is hardly a tragedy.
data SpecForHumans a where
    -- Types of input
    Input :: String -> SpecForHumans ()
    Symlink :: String -> SpecForHumans ()

    -- Types of output
    Output :: String -> SpecForHumans ()
    Surrogate :: String -> SpecForHumans ()

    -- Describe a subdirectory
    Subdir :: String -> SpecForHumans b -> SpecForHumans ()

    -- For circular pegs in a world of square holes
    Note :: String -> SpecForHumans ()

    -- Implementation detail.
    Don'tUse :: SpecForHumans a

-- This type is really just an unholy abuse of GADTs to allow freeform
--  writing in do-syntax.  It is not law-abiding, but who cares?
instance Functor SpecForHumans where
    fmap _ _ = Don'tUse
instance Applicative SpecForHumans where
    pure _ = Don'tUse
    _ <*> _ = Don'tUse
instance Monad SpecForHumans where
    return _ = Don'tUse
    _ >>= _ = Don'tUse

informalSpec :: String -> SpecForHumans a -> App ()
informalSpec _ _ = pure ()
