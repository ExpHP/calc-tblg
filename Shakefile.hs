{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import           Prelude hiding (FilePath)
import           "base" Debug.Trace
import qualified "filepath" System.FilePath.Posix as Shake((</>))
import           "turtle-eggshell" Eggshell hiding (need, root)
import qualified "foldl" Control.Foldl as Fold
import           ShakeUtil
import           JsonUtil

opts :: ShakeOptions
opts = shakeOptions
    { shakeFiles     = ".shake/"
--    , shakeVerbosity = Diagnostic
    , shakeLint      = Just LintBasic
    , shakeReport    = [".shake/report.json"]
    }

main :: IO ()
main = shakeArgs opts $ do

    -- runs make-inputs, which initializes a data directory structure
    -- with a subtree for each moire pattern
    "init:[:**]" ~!> \_ F{..} -> needSurrogate "make-inputs" (file "data/[]")

    -- goes further than init and elaborates each pattern's subtree
    -- with more input files
    "generate:[:**]" ~!> \_ F{..} -> do
        dirs <- directoryTrials (file "data/[]")
        mapM_ (needSurrogate "copy-template") dirs

    surrogate "make-inputs"
        -- NOTE: these lines are aware of the depth of the tree (# of [] and depth number)
        [ ("[]/[]/spatial-params.toml", 3)
        , ("[]/[]/layers.toml", 3)
        , ("[]/[]/positions.json", 3)
        , ("[]/[]/supercells.json", 3)
        , ("shake", 1)
        , ("shakexc", 1)
        , ("update", 1)
        , ("scripts", 1)
        , ("Shakefile.hs", 1)
        , ("input/[].gplot.template", 2)
        ] $ "data/[:**]" #> \root F{..} -> do
            unit $ liftAction $ script "make-inputs"
                "-Iignore-keys -Wonly-keys"
                [ "-o", root, "-S", "general-spatial-params.toml" ]

            liftIO $ do
                cp "shake"              (file "data/[]/shake")
                cp "shakexc"            (file "data/[]/shakexc")
                cp "Shakefile-inner.hs" (file "data/[]/Shakefile.hs")
                cp "update-inner"       (file "data/[]/update")
                eggIO $ cptree "templates/base-input" (file "data/[]/input")
                symlink "../../scripts" (file "data/[]/scripts")

    "copy:[:**]" ~!> \_ F{..} -> needSurrogate "copy-template" (file "[]")

    surrogate "copy-template"
        [ ("input/config.json", 2)
        , ("input/hsym.json", 2)
        ] $ "data/[:**]" #> \root F{..} -> do
            -- NOTE: this line is aware of the depth of the tree (# of parents)
            needSurrogate "make-inputs" ((idgaf.parent.parent.idgaf) root)

            -- NOTE: we deliberately copy this in a manner that does not track deps
            outeract $ mergetreeDumb "templates/sp2-template" (idgaf root)

            eggIO $ eggInDir (idgaf root) $ do
                Just supercell <- getJson "supercells.json" ["phonopy"]
                setJson "input/config.json" ["phonopy", "supercell_dim"] supercell

directoryTrials :: FileString -> Act [FileString]
directoryTrials dir = do
    needSurrogate "make-inputs" dir
    eggIO . fmap traceShowId . fold Fold.list $
        idgaf . parent <$>
        -- NOTE: this line is aware of the depth of the tree (# of * components)
        glob (idgaf (idgaf dir </> "*/*/positions.json"))

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
