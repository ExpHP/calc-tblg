
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Prelude hiding (FilePath)
import           "base" Debug.Trace
import           "shake" Development.Shake
import qualified "filepath" System.FilePath.Posix as Shake((</>))
import           "turtle-eggshell" Eggshell hiding (need, root)
import qualified "foldl" Control.Foldl as Fold
import qualified "text" Data.Text as Text
import           ShakeUtil
import           JsonUtil

opts :: ShakeOptions
opts = shakeOptions
    { shakeFiles     = ".shake/"
    , shakeVerbosity = Diagnostic
        , shakeLint      = Just LintBasic
    , shakeReport    = [".shake/report.json"]
    }

main :: IO ()
main = shakeArgs opts $ do

    -- runs make-inputs, which initializes a data directory structure
    -- with a subtree for each moire pattern
    "init:[:**]" ~!> \_ (Fmt fmt) -> needSurrogate "make-inputs" (fmt "data/[]")

    -- goes further than init and elaborates each pattern's subtree
    -- with more input files
    "generate:[:**]" ~!> \_ (Fmt fmt) -> do
        dirs <- directoryTrials (fmt "data/[]")
        mapM_ (needSurrogate "copy-template") dirs

    surrogate "make-inputs"
        [ ("[]/spatial-params.toml", 2)
        , ("[]/positions.json", 2)
        , ("[]/supercells.json", 2)
        , ("shake", 1)
        , ("update", 1)
        , ("scripts", 1)
        , ("Shakefile.hs", 1)
        ] $ "data/[:**]" #> \root (Fmt fmt) -> do
            () <- script "make-inputs"
                "--suppress-shift-dir -Iignore-keys -Wonly-keys"
                [ "-o", root, "-S", "general-spatial-params.toml" ]

            liftIO $ cp "shake"              (fmt "data/[]/shake")
            liftIO $ cp "Shakefile-inner.hs" (fmt "data/[]/Shakefile.hs")
            liftIO $ cp "update-inner"       (fmt "data/[]/update")
            liftIO $ symlink "../../scripts" (fmt "data/[]/scripts")

    "copy:[:**]" ~!> \_ (Fmt fmt) -> needSurrogate "copy-template" (fmt "[]")

    surrogate "copy-template"
        [ ("input/band.gplot.template", 2)
        , ("input/both.gplot.template", 2)
        , ("input/config.json", 2)
        ] $ "data/[:**]" #> \root (Fmt _) -> do
            needSurrogate "make-inputs" ((idgaf.parent.idgaf) root)

            -- NOTE: we deliberately copy this in a manner that does not track deps
            outeract $ mergetreeDumb "scripts/sp2-template" (idgaf root)

            eggIO $ eggInDir (idgaf root) $ do
                Just supercell <- getJson "supercells.json" ["phonopy"]
                setJson "input/config.json" ["phonopy", "supercell_dim"] supercell

directoryTrials :: FileString -> Action [FileString]
directoryTrials dir = do
    needSurrogate "make-inputs" dir
    eggIO . fmap traceShowId . fold Fold.list $
        idgaf . parent <$>
        glob (idgaf (idgaf dir </> "*/positions.json"))

script :: (_)=> FileString -> m a
script = cmd . ("scripts" Shake.</>)

-- | @cp -a src/* dest@.  "Dumb" because it will fail if the trees have any
--   similar substructure (i.e. src\/subdir and dest\/subdir)
mergetreeDumb :: (_)=> FilePath -> FilePath -> egg ()
mergetreeDumb src dest = egg $ do
    entry <- ls src
    cptree entry (dest <> filename entry)

-- | @cp -a src dest@
cptree :: (_)=> FilePath -> FilePath -> egg ()
cptree src dest = procs "cp" ["-a", idgaf src, idgaf dest] empty

eggInDir :: (_)=> s -> Egg a -> Egg a
eggInDir s e = singularToEgg $ pushd (idgaf s) >>= \() -> liftEgg e
