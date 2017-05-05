{-# LANGUAGE RecordWildCards #-}
-- Shakefile
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}


-- AN APOLOGY TO ALL WHOSE EYES ARE UNFORTUNATE ENOUGH TO GAZE UPON THIS MODULE
--
-- I... think that heading itself actually pretty much sums it about up.


-- NOTES TO A FUTURE SELF:
--
--  *  'enter "pat"' is used to section off regions of the shakefile which
--     involve files strictly within a directory.

--  *  Unfortunately this abstraction is leaky; Ultimately,
--     all paths given to shake must be relative to the shake root.
--
--     The function 'file' transforms a file path, substituting capture groups
--     and adding the prefix. ('fmt' does the same, without the prefix).
--     Additionally, many functions whose name end in "File" accept bare,
--     untransformed file paths.
--
--     When in doubt, check the explicitly documented signature;
--     the alias "Pat" is used where untransformed paths are expected;
--     "FileString" is used otherwise.
--
--     This additional cognitive overhead is unfortunate, but is the sacrifice
--     that was made to give the document some notion of structure.

--  *  Use plain Haskell code for any newly added, pure transformations.
--     Use App/Act (the Shakefile-based layer) for transformations already
--      backed by existing shell scripts.
--     AVOID Eggshell/Egg (the Turtle-based layer) for any new code!!
--     It is horribly broken and cannot detect nonzero exit codes.

--  *  Only types derived from Shake.Action (such as Act) retain dependencies.
--     I.e. anything that runs in an eggshell or egg has no automatically
--          tracked dependencies.

import           ExpHPrelude hiding (FilePath, interact)
import           "base" Data.IORef
import           "base" Data.Function(fix)
import           "base" Data.Complex
import qualified "base" Data.List as List
import           "base" System.IO.Error(isDoesNotExistError)
import qualified "shake" Development.Shake as Shake
import           "shake" Development.Shake.FilePath(normaliseEx)
import qualified "filepath" System.FilePath.Posix as Shake((</>))
import           "directory" System.Directory(createDirectoryIfMissing, removePathForcibly)
import           "exceptions" Control.Monad.Catch(handleIf)
import qualified "containers" Data.Set as Set
import qualified "containers" Data.Map as Map
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.Encoding as Text
import qualified "text" Data.Text.IO as Text.IO
import qualified "text" Data.Text.Read as Text.Read
import qualified "bytestring" Data.ByteString.Lazy as ByteString.Lazy
import           "lens" Control.Lens hiding ((<.>), strict)
import qualified "foldl" Control.Foldl as Fold
import           "vector" Data.Vector((!))
import qualified "vector" Data.Vector as Vector
import qualified "aeson" Data.Aeson as Aeson
import qualified "aeson" Data.Aeson.Types as Aeson
import qualified "lens-aeson" Data.Aeson.Lens as Aeson
import qualified "vasp-poscar" Data.Vasp.Poscar as Poscar
import           "turtle-eggshell" Eggshell hiding (need,view,empty)
import qualified "terrible-filepath-subst" Text.FilePath.Subst as Subst
import qualified Turtle.Please as Turtle hiding (empty)
import           JsonUtil
import           ShakeUtil hiding ((%>))
import qualified Band as Uncross
import qualified Band.Oracle.Phonopy as Uncross
import           Band.Fold(foldBandComputation)

opts :: ShakeOptions
opts = shakeOptions
    { shakeFiles     = ".shake/"
    --, shakeVerbosity = Diagnostic
    , shakeVerbosity = Normal
    , shakeLint      = Just LintFSATrace
    }

-- NOTE: these data types are used for automatic serialization.
--       layout matters!
-- data for gnuplot, indexed by:   band, hsymline, kpoint
type BandGplData a = (Vector (Vector (Vector a)))
type BandGplColumn = BandGplData Double

main :: IO ()
main = shakeArgs opts $ do
    metaRules
    sp2Rules
    oldRules
    plottingRules
    crossAnalysisRules
    miscRules

metaRules :: App ()
metaRules = do
    -- let the user supply their own pattern.
    -- [p], [v], and [k] will iterate over the values they are
    --   usually expected to have in the patterns used in this file.
    "all:[:**]" ~!> \('a':'l':'l':':':pat) _ -> do
        ps <- allPatterns
        vs <- pure ["vdw", "novdw"]
        ks <- pure kpointShorts

        let mapMaybe f xs = f <$> xs >>= maybe [] pure
        let Identity func = (iDontCare . Subst.compile) "[p]:::[v]:::[k]"
                            >>= (iDontCare . flip Subst.substIntoFunc pat)

        let pvks = List.intercalate ":::" <$> sequence [ps,vs,ks]
        -- Shake will do the files in arbitrary order if we need them all
        -- at once which sucks because it is nice to do lowest volume first.
        -- need $ orderPreservingUnique (mapMaybe func pvks)
        mapM_ needs $ orderPreservingUnique (mapMaybe func pvks)

    "reset:[p]" ~!> \_ F{..} -> do
        loudIO $ eggInDir (fmt "[p]") $ do
            forM_ ["vdw", "novdw", ".uncross", ".post", ".surrogate"] $
                liftIO . removePathForcibly

sp2Rules :: App ()
sp2Rules = do
    "[p]/input/[x].gplot.template" `isCopiedFromFile` "input/[x].gplot.template"

    enter "[p]" $ do
        let makeStructure :: [String] -> _
            makeStructure extra path F{..} = do
                posJson    <- needsFile "positions.json"
                paramsToml <- needsFile "spatial-params.toml"

                liftAction $ script "make-poscar" extra
                        "-S" paramsToml "-P" posJson
                        (FileStdout path)

        "input/moire.vasp" !> makeStructure []
        "input/moire.xyz"  !> makeStructure ["--xyz"]

    -- rules involving phonopy use surrogates to hide the fact that they
    -- perform modifications to shared files.
    -- these surrogates must be called in a particular sequence, which is
    -- ensured by their dependencies.
    -- Temporary directories would be a nicer solution.
    enter "[p]" $ do

        -- HACK:  file.ext vs file-true.ext:
        --     - file-true.ext  is the one tracked in Shake's dependency graph
        --     - file.ext       is the actual input to computations, and gets modified
        let configRule lj = \path F{..} -> do
            copyPath (file "input/config.json") path
            loudIO $ setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj

        "vdw/config-true.json"   !> configRule True
        "novdw/config-true.json" !> configRule False
        "[v]/moire-true.[ext]"   `isCopiedFromFile` "input/moire.[ext]"

        ephemeralFile "vdw/moire.vasp"   -- use [v]/relaxed.vasp
        ephemeralFile "novdw/moire.vasp"
        ephemeralFile "vdw/config.json"  -- use [v]/config-true.vasp
        ephemeralFile "novdw/config.json"

        enter "[v]" $ do
            ephemeralFile "band.yaml"

            surrogate "optimization"
                [ ("relaxed.vasp", 1)
                ] $ "" #> \root F{..} -> do

                    copyPath (file "config-true.json") (file "config.json")
                    copyPath (file "moire-true.vasp")  (file "moire.vasp")
                    copyPath (file "moire-true.xyz")   (file "moire.xyz")
                    loudIO . eggInDir root $ doMinimization "moire.vasp"
                    copyUntracked (file "moire.vasp") (file "relaxed.vasp")

            surrogate "displacements"
                [ ("POSCAR-[x]", 1)
                , ("band.conf", 1)
                ] $ "" #> \root F{..} -> do
                    needSurrogate "optimization" root
                    copyPath (file "relaxed.vasp") (file "moire.vasp")
                    loudIO . eggInDir root $ sp2Displacements

            surrogate "force-constants"
                [ ("force_constants.hdf5", 1)
                , ("FORCE_SETS", 1)
                , ("eigenvalues-orig.yaml", 1)
                , ("band_labels.txt", 1)
                ] $ "" #> \root F{..} -> do
                    needSurrogate "displacements" root
                    copyPath (file "relaxed.vasp") (file "moire.vasp")
                    loudIO . eggInDir root $ sp2Forces
                    moveUntracked (file "band.yaml") (file "eigenvalues-orig.yaml")

            surrogate "raman"
                [ ("gauss_spectra.dat", 1)
                ] $ "" #> \root F{..} -> do
                    needSurrogate "force-constants" root
                    copyPath (file "relaxed.vasp") (file "moire.vasp")
                    loudIO . eggInDir root $ sp2Raman

-- Rules currently in disuse (or at least, thought to be).
-- Either the information is no longer of interest, or is obtained through other strategies.
-- (in either case, the reason they're still here is because I am not 100% certain
--   that they will be forever obsolete)
oldRules :: App ()
oldRules = do
    enter "[p]" $ do
        enter "[v]" $ do

            -- band.yaml for eigenvectors at a few specific K points.
            "eigenvectors.yaml" !> \_ F{..} -> do
                needFile ["sc.conf"]
                needFile ["force_constants.hdf5"]

                loudIO . eggInDir (file "") . egg $ do

                    -- looks like [Gamma, Gamma', K, K', M, M', Gamma]
                    -- where x' is a kpoint *just after* x
                    let klocs = result :: [[Double]]
                          where
                            result = insertAfters . appendHead $ fmap kpointLoc kpointShorts
                            insertAfters (a:xs@(b:_)) = a:afterPoint a b:insertAfters xs
                            insertAfters [a] = [a]
                            insertAfters [] = error "buggy bug (insertAfters)"
                            afterPoint a b = zipWith (\a b -> (99*a + b)/100) a b
                            appendHead (x:xs) = (x:xs) ++ [x]
                            appendHead [] = error "buggy bug (appendHead)"

                    let kstr = Text.intercalate " " . fmap repr $ concat klocs

                    procs "phonopy"
                        [ "sc.conf"
                        , "--readfc"
                        , "--eigenvectors"
                        , "--band_points=1"
                        , "--band=" <> kstr
                        ] empty

                    mv "band.yaml" "eigenvectors.yaml"

            "eigenvectors.json" !> \json F{..} -> do
                yaml <- needsFile "eigenvectors.yaml"
                liftAction $ script "eigenvectors-alt --all" yaml (FileStdout json)

            -- FIXME HACK
            "band-at-g.json"    %> \F{..} -> (!!0) <$> needJSONFile "eigenvectors.json" :: Act Aeson.Value
            "band-after-g.json" %> \F{..} -> (!!1) <$> needJSONFile "eigenvectors.json" :: Act Aeson.Value
            "band-at-m.json"    %> \F{..} -> (!!2) <$> needJSONFile "eigenvectors.json" :: Act Aeson.Value
            "band-after-m.json" %> \F{..} -> (!!3) <$> needJSONFile "eigenvectors.json" :: Act Aeson.Value
            "band-at-k.json"    %> \F{..} -> (!!4) <$> needJSONFile "eigenvectors.json" :: Act Aeson.Value
            "band-after-k.json" %> \F{..} -> (!!5) <$> needJSONFile "eigenvectors.json" :: Act Aeson.Value

            "freqs/[k]" !> \freqOut F{..} -> do
                let Just i = List.elemIndex (fmt "[k]") kpointShorts -- HACK should use order in data

                [(_,y)] <- needDataDat [file "data.json"]
                writeJSON freqOut $ fmap ((! i) >>> (! 0)) y

-- Computations that analyze the relationship between VDW and non-VDW
crossAnalysisRules :: App ()
crossAnalysisRules = do

    -- initialize oracles, which talk to phonopy and acquire eigenvectors.
    let needOracles :: FileString -> Act (Uncross.Oracle, Uncross.Oracle)
        needOracles p = do
            let dirs = ((p Shake.</> ".uncross") Shake.</>) <$> ["novdw", "vdw"]
            liftIO . forM_ dirs $ createDirectoryIfMissing True
            need [ v Shake.</> m
                 | v <- dirs
                 , m <- [ "eigenvalues.yaml"
                        , "force_constants.hdf5"
                        , "oracle.conf"
                        , "hsym.json"
                        , "POSCAR"
                        , "FORCE_SETS"
                        ]
                 ]
            [oracleNoVdw, oracleVdw] <- liftIO $ mapM Uncross.initOracle dirs
            pure (oracleNoVdw, oracleVdw)

    -- oracle inputs
    enter "[p]" $ do
        ".uncross/[v]/force_constants.hdf5" `isHardLinkToFile` "[v]/force_constants.hdf5"
        ".uncross/[v]/eigenvalues.yaml"     `isHardLinkToFile` "[v]/eigenvalues-orig.yaml"
        ".uncross/[v]/oracle.conf"          `isHardLinkToFile` "[v]/sc.conf"
        ".uncross/[v]/hsym.json"            `isCopiedFromFile` "input/hsym.json"
        ".uncross/[v]/POSCAR"               `isCopiedFromFile` "[v]/relaxed.vasp"
        ".uncross/[v]/FORCE_SETS"           `isCopiedFromFile` "[v]/FORCE_SETS"
        "[v]/eigenvalues.yaml"              `isHardLinkToFile` ".uncross/[v]/corrected.yaml"

    -- uncross
    enter "[p]" $ do
        enter ".uncross" $ do
            -- -- do uncross
            -- surrogate "run-uncross"
            --     [ ("[v]/corrected.yaml", 2)
            --     ] $ "" #> \root F{..} -> do
            --         liftIO $ createDirectoryIfMissing True root
            --         (oracleNoVdw, oracleVdw) <- needOracles (fmt "[p]")
            --         let cfg = Uncross.UncrossConfig { Uncross.cfgOracleA = oracleNoVdw
            --                                         , Uncross.cfgOracleB = oracleVdw
            --                                         , Uncross.cfgWorkDir = file "work"
            --                                         }
            --         liftIO $ Uncross.runUncross cfg

            -- disable uncross
            surrogate "run-uncross"
                [ ("[v]/corrected.yaml", 2)
                ] $ "" #> \root F{..} -> do
                    copyPath (file "vdw/eigenvalues.yaml") (file "vdw/corrected.yaml")
                    copyPath (file "novdw/eigenvalues.yaml") (file "novdw/corrected.yaml")

    -- 1st order perturbation theory, whose current implementation resembles
    -- some sort of fleshy scab hanging off of the uncrosser code.  Don't pick at it!
    enter "[p]" $ do
        enter ".uncross" $ do
            surrogate "run-perturb-first"
                [ ("[v]/perturb1.yaml", 2)
                ] $ "" #> \root F{..} -> do
            (oracleNoVdw, oracleVdw) <- needOracles (fmt "[p]")
            let sysA = "novdw"
            let sysB = "vdw"
            let density = 100 -- FIXME should be part of input
            hSymPath <- liftIO $ readJSON "hsym.json" :: Act Uncross.HighSymInfo
            let qPath = Uncross.highSymPathToQPath density hSymPath
            energiesA <- liftIO $ Uncross.readQPathEnergies (sysA Shake.</> "eigenvalues.json")
            energiesB <- liftIO $ Uncross.readQPathEnergies (sysB Shake.</> "eigenvalues.json")
            let cfg = Uncross.UncrossConfig { Uncross.cfgWorkDir = file "work"
                                            , Uncross.cfgQPath             = qPath
                                            , Uncross.cfgOriginalEnergiesA = energiesA
                                            , Uncross.cfgOriginalEnergiesB = energiesB
                                            , Uncross.cfgSystemDirA = file sysA -- XXX should be in context of a compute closure
                                            , Uncross.cfgSystemDirB = file sysB
                                            }

            (qs, getNoVdw, getVdw) <- liftIO $ Uncross.makeAnUncrosserSolelyToActAsAPrecomputedEigenvectorCache cfg

            e1s <- Vector.forM (Vector.fromList qs) $ \q -> liftIO $ do
                unperturbedEigs <- getNoVdw q :: IO (Vector Double, Vector (UVector (Complex Double)))
                exactEigs <- getVdw q
                pure . fst $ Uncross.firstOrderPerturb 0 unperturbedEigs exactEigs

            -- no 'file'; this writes to the oracle's dir
            liftIO $ Uncross.askToWriteNamedFile (file "novdw") ("perturb1.yaml") e1s -- XXX func shouldn't exist


    -- band unfolding (or perhaps rather, /folding/)
    let zeroDegreePattern = "1-0-1-1-1-1"
    let unitCompute :: FileString -> [[Double]] -> Act [[Double]]
        unitCompute v = computeStructureBands (zeroDegreePattern Shake.</> v Shake.</> "relaxed.vasp")
                                              (zeroDegreePattern Shake.</> v Shake.</> "force_constants.hdf5")
                                              (zeroDegreePattern Shake.</> v Shake.</> "sc.conf")

    enter "[p]" $ do
        enter "[v]" $ do
            "folded-ab.yaml" !> \_ F{..} -> do

                cMat <- patternCMatrix (fmt "[p]")
                let superCompute = (fmap sort) . foldBandComputation cMat (unitCompute (fmt "[v]"))

                -- FIXME HACK IMSORRY
                _ <- needs (fmt $ "[p]/.uncross/[v]/hsym.json")
                _ <- needs (fmt $ "[p]/.uncross/[v]/eigenvalues.yaml")
                qs <- concat . fmap (fmap toList) . fmap toList . toList . Uncross.qPathDataByLine
                      <$> liftIO (Uncross.abuseOracleFrameworkToGetKPath (fmt "[p]/.uncross/[v]"))

                -- write the unfolded band structure into a valid band.yaml
                es <- superCompute qs
                readModifyWrite (Uncross.putBandYamlSpectrum (Vector.fromList . fmap Vector.fromList $ es))
                                (needsFile "eigenvalues-orig.yaml" >>= liftIO . readYaml)
                                (liftIO . writeYaml (file "folded-ab.yaml"))

    -- connect the dots; assimilate all this data into the codebase
    enter "[p]" $ do
        "vdw/perturb1.yaml" `isHardLinkToFile` ".uncross/vdw/perturb1.yaml"
        "[v]/data-folded-ab.dat" `datIsConvertedFromYaml` "[v]/folded-ab.yaml"
        "vdw/data-perturb1.dat"  `datIsConvertedFromYaml` "vdw/perturb1.yaml"

    pure ()

-- oddball deps
miscRules :: App ()
miscRules = do

    -- Parse gnuplot into JSON with high-symmetry point first
    enter "[p]" $ do
        enter "[v]" $ do

            "data-orig.dat" `datIsConvertedFromYaml` "eigenvalues.yaml"

            -- Jsonification
            "data-[x].json" !> \json F{..} -> do
                dat <- needsFile "data-[x].dat"
                liftAction $ degnuplot (FileStdin dat) (FileStdout json)

            -- HACK: we used to do band uncrossing here, but no longer. just hard link
            "data.json" `isHardLinkToFile` "data-orig.json"
            "data.dat"  `isHardLinkToFile` "data-orig.dat"

    enter "[p]" $ do
        enter "[v]" $ do

            --------------------
            -- a phonopy input file with just the supercell
            "sc.conf" !> \scConf F{..} -> do
                copyPath (file "band.conf") scConf
                appendLines scConf ["HDF5 = .TRUE."]

plottingRules :: App ()
plottingRules = do
    enter "[p]" $ do

        -- Input data files, which may or may not have associated plots
        -- (those without may contain 'src' in the name)
        ".post/bandplot/data-novdw.[ext]"             `isHardLinkToFile` "novdw/data.[ext]"
        ".post/bandplot/data-vdw.[ext]"               `isHardLinkToFile` "vdw/data.[ext]"
        ".post/bandplot/data-src-[v]-folded-ab.[ext]" `isHardLinkToFile` "[v]/data-folded-ab.[ext]"
        ".post/bandplot/data-src-perturb1.[ext]"      `isHardLinkToFile` "vdw/data-perturb1.[ext]"
        enter ".post/bandplot" $ do
            "data-both.dat" !> \dataBoth F{..} -> do
                [(xs, ys0)] <- needDataDat [file "data-novdw.json"]
                [(_,  ysV)] <- needDataDat [file "data-vdw.json"]
                [(_,  ys1)] <- needDataDat [file "data-src-perturb1.json"]
                let out = idgaf $ Aeson.encode [xs, ys0, ysV, ys1]
                liftAction $ engnuplot (Stdin out) (FileStdout dataBoth)

            "data-[v]-folded-ab.dat" !> \dataBoth F{..} -> do
                [(xs, ys0)] <- needDataDat [file "data-[v].json"]
                [(_,  ys1)] <- needDataDat [file "data-src-[v]-folded-ab.json"]
                let out = idgaf $ Aeson.encode [xs, ys0, ys1]
                liftAction $ engnuplot (Stdin out) (FileStdout dataBoth)

            "data-both.dat" !> \dataBoth F{..} -> do
                [(xs, ys0)] <- needDataDat [file "data-novdw.json"]
                [(_,  ysV)] <- needDataDat [file "data-vdw.json"]
                [(_,  ys1)] <- needDataDat [file "data-src-perturb1.json"]
                let out = idgaf $ Aeson.encode [xs, ys0, ysV, ys1]
                liftAction $ engnuplot (Stdin out) (FileStdout dataBoth)

            let okLine l = case words l of
                    [_, y1, y2] | (log (abs (read y1 - read y2)) :: Double) < -4 -> False
                    _ -> True

            "data-filter.dat" !> \dataFilter F{..} ->
                readModifyWrite (filter (okLine . idgaf))
                                (readLines (file "data-both.dat"))
                                (writeLines dataFilter)

            "data-num-[i].dat" !> \dataBoth F{..} -> do
                (xs, ysN) <- needJSONFile "data-novdw.json" :: Act ([[[Double]]], [[[Double]]])
                (_,  ysV) <- needJSONFile "data-vdw.json"   :: Act ([[[Double]]], [[[Double]]])
                xs  <- pure $ fmap (List.transpose . (:[]) . (!! read (fmt "[i]")) . List.transpose) xs
                ysN <- pure $ fmap (List.transpose . (:[]) . (!! read (fmt "[i]")) . List.transpose) ysN
                ysV <- pure $ fmap (List.transpose . (:[]) . (!! read (fmt "[i]")) . List.transpose) ysV
                let out = idgaf $ Aeson.encode [xs, ysN, ysV]
                liftAction $ engnuplot (Stdin out) (FileStdout dataBoth)

            "data-perturb1.dat" `isHardLinkToFile` "data-both.dat"


    enter "[p]" $ do
        let gplotXBase :: _ -> _ -> _ -> Act [String]
            gplotXBase dataFile titleFile ticksFile = do
                xticksLine <- readPath ticksFile
                title <- readPath titleFile

                let dquote = \s -> "\"" ++ s ++ "\""
                pure
                    [ "set title " ++ dquote (idgaf title)
                    , xticksLine
                    , "band_n = 3" -- FIXME
                    , "data = " ++ dquote ((idgaf.filename.idgaf) dataFile)
                    ]

        ".post/bandplot/title" !> \title F{..} ->
                readModifyWrite head (readLines (file "input/moire.vasp"))
                                     (writePath title)
        ".post/bandplot/data-prelude.dat" !> \prelude F{..} ->
                readModifyWrite (take 3) (readLines (file "vdw/data-orig.dat"))
                                         (writeLines prelude)

        ".post/bandplot/band_labels.txt"              `isCopiedFromFile` "vdw/band_labels.txt"
        ".post/bandplot/vdw.gplot.template"           `isCopiedFromFile` "input/band.gplot.template"
        ".post/bandplot/novdw.gplot.template"         `isCopiedFromFile` "input/band.gplot.template"
        ".post/bandplot/both.gplot.template"          `isCopiedFromFile` "input/both.gplot.template"
        ".post/bandplot/filter.gplot.template"        `isCopiedFromFile` "input/filter.gplot.template"
        ".post/bandplot/[v]-folded-ab.gplot.template" `isCopiedFromFile` "input/folded.gplot.template"
        ".post/bandplot/perturb1.gplot.template"      `isCopiedFromFile` "input/perturb1.gplot.template"
        ".post/bandplot/num-[i].gplot.template"       `isCopiedFromFile` "input/both.gplot.template"

        enter ".post/bandplot" $ do
            "band_xticks.txt" !> \xvalsTxt F{..} -> do
                -- third line has x positions.  First character is '#'.
                dataLines <- readLines (file "data-prelude.dat")
                let counts = List.words . tail $ idgaf (dataLines !! 2)
                labels <- List.words <$> readPath (file "band_labels.txt")

                let dquote = \s -> "\"" ++ s ++ "\""
                let paren  = \s -> "("  ++ s ++ ")"
                writePath xvalsTxt
                    ("set xtics " ++ paren
                        (List.intercalate ", "
                            (List.zipWith (\l a -> dquote l ++ " " ++ a)
                                labels counts)))

            "[s].gplot" !> \bandGplot F{..} -> do
                topLines <- gplotXBase <$> needsFile "data-[s].dat"
                                       <*> needsFile "title"
                                       <*> needsFile "band_xticks.txt"
                                       & join
                readModifyWrite ((topLines <>) . fmap idgaf)
                                (readLines (file "[s].gplot.template"))
                                (writeLines bandGplot)

    -- animations (this is old)
    enter "[p]" $ do

        "out/wobble/[k]-[b]-[v].xyz" !> \outXyz F{..} -> do
            bandYaml <- needsFile "[v]/band-[k].yaml"
            Just supercell <- getJson (file "supercells.json") ["wobble"]
            supercell <- pure $ (supercell ^.. Aeson.values . Aeson._Integral :: [Int])

            liftAction $ cmd "wobble" bandYaml
                    "--supercell" (show supercell)
                    "--bands"     (fmt "[b]")
                    (FileStdout outXyz)

    enter "[p]" $
        enter ".post/bandplot" $
            family
                [ "[s].png"
                , "[s].svg"
                ] &!> \_ F{..} -> do
                    gplot <- needsFile "[s].gplot"
                    () <- liftAction $
                        cmd "gnuplot" (Cwd $ file "") (FileStdin gplot)
                    moveUntracked (file "band.png") (file "[s].png")
                    moveUntracked (file "band.svg") (file "[s].svg")

    -- "out/" for collecting output across all patterns
    liftIO $ createDirectoryIfMissing True "out/bands"
    "out/bands/[p]_[s].[ext]" `isCopiedFromFile` "[p]/.post/bandplot/[s].[ext]"


-- gnuplot data is preparsed into JSON, which can be parsed much
--   faster by any python scripts that we still use.
-- (and infinitely easier to work with in the python repl)
degnuplot :: PartialCmd
degnuplot = script "degnuplot -Gbhkx -Jxhkb"
engnuplot :: PartialCmd
engnuplot = script "engnuplot -Gbhkx -Jxhkb"

data SerdeFuncs a = SerdeFuncs
  { sfRule :: Pat -> (Fmts -> Act a) -> App ()
  , sfNeed :: [FileString] -> Act [a]
  , sfNeedFile :: [FileString] -> Act [a]
  }


SerdeFuncs { sfRule=dataDatRule
           , sfNeed=needDataDat
           } = serdeFuncs :: SerdeFuncs (BandGplColumn, BandGplColumn)

-- Make a pair of `!>` and `need` functions that work with a JSON serialized
-- datatype.
serdeFuncs :: (Aeson.ToJSON a, Aeson.FromJSON a)=> SerdeFuncs a
serdeFuncs = SerdeFuncs sfRule sfNeed sfNeedFile
  where
    sfRule = (%>)
    sfNeed paths = need paths >> forM paths readJSON
    sfNeedFile paths = needFile paths >> forM paths (askFile >=> readJSON)

writeJSON :: (Aeson.ToJSON a, MonadIO io)=> FileString -> a -> io ()
writeJSON dest value = liftIO $ ByteString.Lazy.writeFile dest (Aeson.encode value)
readJSON :: (Aeson.FromJSON a, MonadIO m)=> FileString -> m a
readJSON s = do
    value <- Aeson.eitherDecode <$> liftIO (ByteString.Lazy.readFile s)
    either fail pure value

needJSON :: (_)=> FileString -> Act a
needJSON s = head <$> sfNeed serdeFuncs [s]
needJSONFile :: (_)=> FileString -> Act a
needJSONFile s = head <$> sfNeedFile serdeFuncs [s]

(%>) :: _ => Pat -> (Fmts -> Act a) -> App ()
pat %> act = pat !> \dest fmts ->
    act fmts >>= writeJSON dest

-- this awkward pattern pops up every now and then in my code and it never looked very readable to me
--  except in the most trivial cases
readModifyWrite :: (Monad m)=> (a -> b) -> m a -> (b -> m c) -> m c
readModifyWrite f read write = f <$> read >>= write

------------------------------------------------------------
-- TODO I would like heavier use of temp directories, like in the functions found here.
--      (once I can figure out how to stop forgetting to write "tmp </>", that is...)

-- Operator to create a band.yaml -> data.dat rule.
datIsConvertedFromYaml :: Pat -> Pat -> App ()
datIsConvertedFromYaml dataPat yamlPat = do
    dataPat !> \dataDat F{..} -> do
        withTempDir $ \tmp -> do
            eigYaml <- needsFile yamlPat
            copyPath (idgaf eigYaml) (tmp Shake.</> "band.yaml")

            liftAction $ cmd "bandplot --gnuplot" (Cwd tmp) (FileStdout dataDat)

-- Ask phonopy for eigenvalues, using a temp dir to preserve our sanity.
computeStructureBands :: FileString -- POSCAR
                      -> FileString -- force_constants.hdf5
                      -> FileString -- configFile
                      -> [[Double]] -- qpoints (fractional recip. space)
                      -> Act [[Double]] -- ascending frequncies (THz) at each qpoint
computeStructureBands fpPoscar fpForceConstants fpConf qs =
    withTempDirDebug $ \tmp -> do
        linkPath fpPoscar         (tmp Shake.</> "POSCAR")
        linkPath fpForceConstants (tmp Shake.</> "force_constants.hdf5")
        copyPath fpConf           (tmp Shake.</> "band.conf")

        let qs3d = fmap (take 3 . (++ repeat 0)) qs ++ [[0,0,0]]
        liftIO $ readFile (tmp Shake.</> "band.conf") >>= traceIO . idgaf
        appendLines (tmp Shake.</> "band.conf")
            [ "BAND_POINTS = 1"
            , "BAND = " ++ List.unwords (show <$> concat qs3d)
            ]

        () <- liftAction $ cmd "phonopy --hdf5 --readfc band.conf" (Cwd tmp)


        -- _ <- liftIO $ fmap toList . toList . Uncross.getBandYamlSpectrum <$> readYaml (tmp Shake.</> "band.yaml")
        -- fail "x_x"
        liftIO $ fmap toList . toList . Uncross.getBandYamlSpectrum <$> readYaml (tmp Shake.</> "band.yaml")

------------------------------------------------------------

moveUntracked :: (MonadIO io)=> FilePath -> FilePath -> io ()
moveUntracked = mv
copyUntracked :: (MonadIO io)=> FilePath -> FilePath -> io ()
copyUntracked = cp

allPatterns :: Act [FileString]
allPatterns = getPatternStrings >>= sortOnM patternVolume
  where
    getPatternStrings =
        loudIO . fold Fold.list $ (reverse . List.dropWhile (== '/') . reverse) .
            normaliseEx . idgaf . parent <$> glob "*/positions.json"

sortOnM :: (Monad m, Ord b)=> (a -> m b) -> [a] -> m [a]
sortOnM f xs = do
    keys <- mapM f xs
    pure $ map fst $ List.sortBy (comparing snd) $ zip xs keys

-- FIXME I wanted to read these from JSON now that we're out of the bash tarpit.
--       (In fact, dynamically-generated dependencies is Shake's specialty!)
kpointShorts :: [[Char]]
kpointShorts = ["g", "m", "k"]
kpointLoc :: (Fractional t, IsString a, Eq a) => a -> [t]
kpointLoc "g" = [0, 0, 0]
kpointLoc "m" = [1/2, 0, 0]
kpointLoc "k" = [1/3, 1/3, 0]
kpointLoc _   = error "bugger off ghc"


patternVolume :: FileString -> Act Int
patternVolume p = do
    result <- maybe undefined id <$> readJSON (p Shake.</> "positions.json")
    pure . maybe undefined id . flip Aeson.parseMaybe result $
        (Aeson..: "meta") >=> (Aeson..: "volume") >=> (Aeson..: "A")

patternCMatrix :: FileString -> Act [[Int]]
patternCMatrix p = do
    result <- maybe undefined id <$> readJSON (p Shake.</> "positions.json")
    pure . maybe undefined id . flip Aeson.parseMaybe result $
        (Aeson..: "meta") >=> (Aeson..: "C")

--    "//irreps-*.yaml" !> \dest [stem, kpoint] -> runPhonopyIrreps stem kpoint dest

-- common pattern to perform an egg in a directory
eggInDir :: (_)=> s -> Egg a -> Egg a
eggInDir s e = singularToEgg $ pushd (idgaf s) >>= \() -> liftEgg e

script :: FileString -> PartialCmd
script x = cmd ("scripts" Shake.</> x)

------------------------------------------------------

doMinimization :: FilePath -> Egg ()
doMinimization original = do
                             -- NOTE: Lattice parameter minimization is currently disabled because
                             --  it took forever on some structures and wrecked them in the process.
                             --
                             -- (TODO: I think it was actually only the AA-stacked structure which had
                             --  trouble, which kinda makes sense; isn't it metastable?
                             --  If we just elminate that one pattern we can probably reenable this)
                             init
                             ref <- liftIO (newIORef 1.0)
                             _ <- goldenSearch (<) (objective ref)
                                               -- enable lattice param minimization:
                                               -- (1e-3) (0.97,1.03)
                                               -- disable lattice param minimization:
                                               (1) (1.00000, 1.0000001)
                             pure ()
    where
    init :: Egg ()
    init = do
        cp original "POSCAR"
        setJson "config.json" ["structure_file"] $ Aeson.String "POSCAR"

    objective :: IORef Double -> Double -> Egg Double
    objective ref scale = do

        -- ref stores prev scale, since sp2 normalizes scale to 1
        prevScale <- liftIO $ readIORef ref
        liftIO $ writeIORef ref scale

        -- reuse relaxed as guess
        liftIO $ Text.IO.readFile "POSCAR"
                >>= pure . Poscar.fromText
                >>= pure . (\p -> p {Poscar.scale = Poscar.Scale (scale / prevScale)})
                >>= pure . Poscar.toText
                >>= Text.IO.writeFile "POSCAR"

        infos <- fold Fold.list $ thisMany 2 $ liftEgg sp2Relax

        echo $ "================================"
        echo $ "RELAXATION SUMMARY AT s = " <> repr scale
        egg $ do
            (i, RelaxInfo energies) <- select (zip [1..] infos)
            echo $ format (" Trial "%d%": "%d%" iterations, V = "%f)
                            i (length energies) (last energies)
        echo $ "================================"
        pure $ last . last . fmap (\(RelaxInfo x) -> x) $ infos

type GsState x y = (x, x, y)
goldenSearch :: (y -> y -> Bool)
             -> (Double -> Egg y)
             -> Double
             -> (Double,Double)
             -> Egg (Double,y)
goldenSearch isBetterThan f absXTol (lo,hi) = shell where

    -- s <- init lo hi
    -- s >>= step >>= either pure _
    -- s >>= step >>= either pure (\s -> s >>= step >>= either pure _)
    -- s >>= step >>= either pure (\s -> s >>= step >>= either pure (\s -> ...))
    -- ......... O_o .......
    shell = fix (\rec -> (>>= step) >>> (>>= either pure rec)) (init lo hi)

    -- Revelations:
    --  1. In common implementations of the algorithm (such as those on wikipedia)
    --     the values of the function at the endpoints are never used.
    --     (Technically they COULD be used to check curvature, but it is rare that
    --      we wouldn't know whether we are seeking a min or max!)
    --     Hence **it is only necessary to save one y value.**
    --  2. TECHNICALLY the step function doesn't even even need to use phi;
    --      one could record 'b' and derive the second endpoint as 'c = d - b + a'.
    --     But I don't know if that is numerically stable, so we will do what
    --     the wikipedia implementations do and recompute b and c every iter.
    phi = (1 + sqrt 5) / 2
    getb a d = d - (d - a) / phi
    getc a d = a + (d - a) / phi

    -- The interval looks like this:     a----------b----c----------d
    --                 or like this:     d----------c----b----------a
    init :: Double -> Double -> Egg (GsState Double _)
    init a d = f (getb a d)
               >>= \fb -> pure (a, d, fb)

    step :: (GsState Double _) -> Egg (Either (Double,_) (Egg (GsState Double _)))
    step (a, d, fb) = pure $
        let b = getb a d in
        let c = getc a d in
        if abs (b - c) < absXTol
            then Left (b,fb)
            else Right $ f c >>= \fc ->
                if fb `isBetterThan` fc
                    then pure (c, a, fb)
                    else pure (b, d, fc)

data RelaxInfo = RelaxInfo
        { relaxInfoStepEnergies :: [Double]
        } deriving (Show, Eq)

sp2 :: Egg ()
sp2 = procs "sp2" [] empty

sp2Relax :: Egg RelaxInfo
sp2Relax = (setPhonopyState False False False True 1000 >>) . liftEgg $ do
    (out,_) <- addStrictOut sp2
    pure $ parseRelaxInfoFromSp2 out

sp2Displacements :: Egg ()
sp2Displacements = setPhonopyState True False False False 1 >> sp2

sp2Forces :: Egg ()
sp2Forces = setPhonopyState False True True False 1 >> sp2

sp2Raman :: Egg ()
sp2Raman = setPhonopyState False False False True 1 >> sp2

setPhonopyState :: (_)=> Bool -> Bool -> Bool -> Bool -> Int -> io ()
setPhonopyState d f b r c = liftIO $ do
    setJson "config.json" ["phonopy", "calc_displacements"] $ Aeson.Bool d
    setJson "config.json" ["phonopy", "calc_force_sets"] $ Aeson.Bool f
    setJson "config.json" ["phonopy", "calc_bands"] $ Aeson.Bool b
    setJson "config.json" ["phonopy", "calc_raman"] $ Aeson.Bool r
    setJson "config.json" ["relax_count"] $ Aeson.Number (fromIntegral c)

parseRelaxInfoFromSp2 :: Text -> RelaxInfo
parseRelaxInfoFromSp2 =
    -- sp2's output is pretty varied so we use a very specific (and therefore fragile) filter
    Text.lines >>> filter ("    Value:" `Text.isPrefixOf`)
    >>> fmap (Text.words >>> (!! 1) >>> Text.Read.double >>> either error fst) >>> RelaxInfo

logStatus :: (_)=> Text -> egg ()
logStatus ss = pwd >>= echo . format ("== "%s%" at "%fp) ss

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
