{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module ShakeUtil where

import           Prelude hiding (FilePath, interact)
import           "base" Data.String(IsString(..))
import           "base" Control.Monad
import           "base" Debug.Trace
import qualified "base" Data.List as List
import           "mtl" Control.Monad.Identity
import           "shake" Development.Shake hiding (need,needed,want)
import qualified "shake" Development.Shake as Shake
import           "transformers" Control.Monad.Trans.Reader(ReaderT(..))
import qualified "transformers" Control.Monad.Trans.Reader as ReaderT
import           "transformers" Control.Monad.Trans(MonadTrans(..))
import           "base" Control.Monad.IO.Class(MonadIO)
import           "mtl" Control.Monad.Reader(MonadReader)
import qualified "filepath" System.FilePath.Posix as Shake((</>))
import           "shake" Development.Shake.FilePath(normaliseEx)
import qualified "terrible-filepath-subst" Text.FilePath.Subst as Subst

type Pat = String
type FileString = String

-- it would be nice if this could just be a type alias so that we
-- didn't have to keep unpacking it; but then there end up being
-- corner cases where attempting to use the type somewhere may result
-- in an "impredictive type" (which GHC cannot handle)
newtype Fmt = Fmt (forall b. (IsString b)=> Pat -> b)

-- Depend on paths, then return them for the purpose of name binding.
needs :: (IsString string)=> FileString -> Action string
needs path = need [path] >> pure (fromString path)

surrogateRoot :: FileString -> FileString
surrogateRoot = normaliseEx . (++ "/../..")

pushFileRegex :: String -> Pat -> Pat
pushFileRegex name regex =
    -- don't use </> here; we don't want to destroy directory wildcards
    case last regex of '/' -> regex ++ name
                       _   -> regex ++ "/" ++ name

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
surrogate surrogateName filesMade (SurrogateSpec regex userFunc) = do

    -- produce surrogate file along with running actual rule
    let surrogateRegex = pushFileRegex (".surrogate" Shake.</> surrogateName) regex
    surrogateRegex !> \sfile fmt ->
        -- give the user function the path THEIR regex matched, not ours
        let root = normaliseEx (sfile Shake.</> "../..")
        in userFunc root fmt
           >> liftIO (writeFile sfile "")

    -- make created files "depend" on the surrogate
    forM_ filesMade $ \(name,compCount) ->
        pushFileRegex name regex !> \f _ ->
            -- hrrng, now we must recover the surrogate file from the path
            let root = normaliseEx $ foldl (Shake.</>) f (replicate compCount "..") in
            needSurrogate surrogateName root

-- Part of the syntax of 'surrogate'.
data SurrogateSpec = SurrogateSpec Pat (FileString -> Fmt -> Action ())
(#>) :: Pat -> (FileString -> Fmt -> Action ()) -> SurrogateSpec
(#>) = SurrogateSpec

need :: [FileString] -> Action ()
need s = wrapTraceIO ("need " ++ show s) (Shake.need s)
needed :: [FileString] -> Action ()
needed s = wrapTraceIO ("needed " ++ show s) (Shake.needed s)
want :: [FileString] -> App ()
want s = wrapTraceIO ("want " ++ show s) (Shake.want s)

needSurrogate :: FileString -> FileString -> Action ()
needSurrogate = _needSurrogateImpl need

neededSurrogate :: FileString -> FileString -> Action ()
neededSurrogate = _needSurrogateImpl needed

_needSurrogateImpl :: ([FileString] -> Action ())
                   -> FileString -> FileString -> Action ()
_needSurrogateImpl need name root =
    let sfile = root Shake.</> ".surrogate" Shake.</> name
    in need [sfile]

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
(!>) :: Pat -> (String -> Fmt -> Action ()) -> App ()
(!>) pat = \act -> actOnPath $ \path ->
        case Subst.substAgainstFunc subst path of
            Nothing -> Nothing
            Just f  -> Just . wrapTraceIO ("'" ++ pat ++ "' on '" ++ path)
                       $ act path (Fmt $ fromString . either fail id . f)
    where
        Identity subst = Subst.compile pat

-- Make a phony command that operates on subst patterns.
-- This can be used to supply phony commands with arguments.
(~!>) :: Pat -> (String -> Fmt -> Action ()) -> App ()
(~!>) pat = \act -> phonys $ \path ->
        case Subst.substAgainstFunc subst path of
            Nothing -> Nothing
            Just f  -> Just . wrapTraceIO ("'" ++ pat ++ "' on '" ++ path)
                       $ act path (Fmt $ fromString . either fail id . f)
    where
        Identity subst = Subst.compile pat

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
family = id

-- NOTE: Strictly speaking, 'family' and 'PatFamily' are not necessary;
--       we could have gotten away with just the operator.
--       I just have them for symmetry with 'surrogate', and to make the
--         indentation forced by do-block syntax less distracting.
-- | Part of the syntax for 'family.'
(&!>) :: [Pat] -> ([FileString] -> Fmt -> Action ()) -> App ()
pats &!> act = do
    mapM_ id $ List.zipWith (isSideProductOf) pats (drop 1 pats)
    last pats !> \_ (Fmt fmt) -> act (fmt <$> pats) (Fmt fmt)

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

actOnPath :: (FileString -> Maybe (Action ())) -> App ()
actOnPath f =  (maybe False (const True)     . f)
            ?> (maybe (error "actOnPath") id . f)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

-- We're abusing this option for science.
dontTrack :: Action a -> Action a
dontTrack = orderOnlyAction

-- Declare that the LHS is simply copied from the RHS.
-- (the RHS will be tracked for changes)
isCopiedFrom :: Pat -> Pat -> App ()
isCopiedFrom opat ipat = opat !> \path (Fmt fmt) -> copyFileChanged (fmt ipat) path

-- Declare that one file is always produced alongside another.
-- (this makes the LHS simply depend on the RHS)
isSideProductOf :: Pat -> Pat -> App ()
isSideProductOf opat ipat = opat !> \_ (Fmt fmt) -> need [fmt ipat]

-- withTempDir' :: (Pat -> )[TempDirAttr] -> (FilePath -> Action ()) -> Rules ()
-- withTempDir' attrs act = withTempDir $ \dir -> do
--     -- track deps (copyFile') when copying in
--     mapM_ (\(s,t,_) -> copyFile' s (dir </> t))
--           (filter (\(_,_,c) -> c == TempDirInput) attrs)
--     act dir
--     -- don't track deps (copyFile) when copying out
--     mapM_ (\(s,t,_) -> copyFile (dir </> s) t)
--           (filter (\(_,_,c) -> c == TempDirOutput) attrs)

-- (>##>) :: Pat -> Pat -> TempDirAttr
-- s >##> t = (s, t, TempDirInput)
-- (<##<) :: Pat -> Pat -> TempDirAttr
-- s <##< t = (s, t, TempDirOutput)

-- type TempDirAttr = (FilePath, FilePath, TempDirAttrKind)
-- data TempDirAttrKind = TempDirInput | TempDirOutput deriving (Eq, Show)


---------------------------------------


-- newtype App a = App { runApp :: ReaderT Config Rules a }
-- data Config = Config { cfgPrefix :: Pat
--                      } deriving (Show, Eq, MonadLift, MonadIO, MonadReader)


-- enter :: Pat -> App a -> App a
-- enter pat app =
--     let f = runReaderT . runApp $ app in
--     App . ReaderT $ \cfg@Config{cfgPrefix=prefix} ->
--                        f cfg{cfgPrefix=prefix Shake.</> pat}
