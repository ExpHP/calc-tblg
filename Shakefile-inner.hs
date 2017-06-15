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

--  *  Some rules are "componentized" to make them more easily reusable.
--     These rules generally work on prefixes of form
--         "some-dedicated-subtree/[c]/[x]/"
--     where [c] disambiguates between "instances" of a component
--     (i.e. different reasons for using the same component)
--     and [x] provides some limited form of namespacing for files within an instance.
--     (generally, each instance will have its own namespacing scheme)

--  *  The 'informalSpec' blocks are no-ops.  Regard them as comments.

import           ExpHPrelude hiding (FilePath, interact)
import           "base" Data.IORef
import           "base" Data.Function(fix)
import           "base" Control.Monad.Zip
import qualified "base" Data.List as List
import           "shake" Development.Shake.FilePath(normaliseEx)
import           "filepath" System.FilePath.Posix((</>))
import           "directory" System.Directory(createDirectoryIfMissing, removePathForcibly, listDirectory, doesFileExist)
import qualified "containers" Data.Set as Set
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.IO as Text
import qualified "text" Data.Text.Read as Text.Read
import qualified "bytestring" Data.ByteString.Lazy as ByteString.Lazy
import           "lens" Control.Lens hiding ((<.>), strict, As)
import qualified "foldl" Control.Foldl as Fold
import qualified "vector" Data.Vector as Vector
import qualified "aeson" Data.Aeson as Aeson
import qualified "aeson" Data.Aeson.Types as Aeson
import           "extra" Control.Monad.Extra(whenJustM, findM)
import qualified "vasp-poscar" Data.Vasp.Poscar as Poscar
import           "turtle-eggshell" Eggshell hiding (need,view,empty,(</>))
import qualified "terrible-filepath-subst" Text.FilePath.Subst as Subst

-- import qualified Turtle.Please as Turtle hiding (empty)
import           JsonUtil
import           GeneralUtil(onlyUniqueValue,reallyAssert)
import           FunctorUtil(wrapZip, unwrapZip, ffmap, fffmap, ffffmap)
import           ShakeUtil hiding ((%>), doesFileExist)
import qualified Band as Uncross
import qualified Phonopy.Types as Phonopy
import qualified Phonopy.IO as Phonopy
import qualified Phonopy.EigenvectorCache as Eigenvectors
import           Band.Fold(foldBandComputation, unfoldBandComputation)

shakeCfg :: ShakeOptions
shakeCfg = shakeOptions
    { shakeFiles     = ".shake/"
    -- , shakeVerbosity = Diagnostic
    , shakeVerbosity = Chatty
    --, shakeVerbosity = Normal
    -- , shakeLint      = Just LintFSATrace
    }

appCfg :: AppConfig
appCfg = appDefaultConfig
    { appDebugMatches = False
    , appDebugRewrite = False
    }

-- NOTE: these data types are used for automatic serialization.
--       layout matters!
-- data for gnuplot, indexed by:   band, hsymline, kpoint
type VVVector a = Vector (Vector (Vector a))
type BandGplData a = VVVector a
type BandGplColumn = BandGplData Double

main :: IO ()
main = shakeArgsWith shakeCfg appCfg [] appFromArgs

allRules :: App ()
allRules = do
    mainRules
    metaRules
    sp2Rules
    plottingRules
    crossAnalysisRules

-------------------------------------------

touchOneMetaruleName :: String
touchOneMetaruleName = "keep"
touchAllMetaruleName :: String
touchAllMetaruleName = "keep-all"

metaRules :: App ()
metaRules = do
    -- let the user supply their own pattern.
    -- [p], [v], and [k] will iterate over the values they are
    --   usually expected to have in the patterns used in this file.
    "all:[:**]" ~!> \_ F{..} -> all_NeedVer (fmt "[]")

    "reset:[p]" ~!> \_ F{..} -> do
        loudIO $ eggInDir (fmt "[p]") $ do
            forM_ ["vdw", "novdw", ".uncross", ".post", ".surrogate"] $
                liftIO . removePathForcibly

    -- remove a single file
    "rm:[:**]" ~!> \_ F{..} ->
        liftIO (removePathForcibly (fmt "[]"))

    -- clean up all the .post directories.
    "clean-post" ~!> \_ F{..} ->
        quietly $ -- FIXME uhhh. 'quietly' isn't working? enjoy the console spam I guess...
            () <$ needs "all:rm:[p]/.post"

    -- meta rules handled during arg parsing
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

namedSaveDir :: FileString -> FileString
namedSaveDir = ("../saves" </>)
filesAffectedBySaving :: IO [FileString]
filesAffectedBySaving = toList . (`Set.difference` blacklist) . Set.fromList <$> listDirectory "."
  where
    blacklist = Set.fromList ["Shakefile.hs", "shake"]

sp2Rules :: App ()
sp2Rules = do

    informalSpec "input" $ do
        Output "hsym.json"
        Subdir "pat/[p]/[a]" $ do
            Output "spatial-params.toml"
            Output "layers.toml"
            Output "positions.json"
            Output "supercells.json"
            Output "input/config.json"
        Output "gplot-helper/[x].gplot"
        Output "gplot-templates/[x].gplot.templates"

    ------------------------------------
    ------------------------------------

    informalSpec "assemble" $ do
        Input "spatial-params.toml"
        Input "layers.toml"
        --------------
        Output "moire.vasp"
        Output "moire.xyz"

    enter "assemble/[c]/[x]" $ do
        let makeStructure :: [String] -> _
            makeStructure extra path F{..} = do
                paramsToml <- needsFile "spatial-params.toml"
                layersToml <- needsFile "layers.toml"

                liftAction $ script "make-poscar"
                                    extra
                                    [layersToml, paramsToml]
                                    (FileStdout path)

        "moire.vasp" !> makeStructure []
        "moire.xyz"  !> makeStructure ["--xyz"]

    ------------------------------------
    ------------------------------------

    informalSpec "sp2" $ do
        Input "config.json"    -- sp2 config
        Input "moire.vasp"     -- structure
        --------------
        Output "relaxed.vasp"  -- structure after relaxation

        Output "disp.conf"
        Output "disp.yaml"

        Output "FORCE_SETS"
        Output "force_constants.hdf5"
        Output "eigenvalues.yaml"
        Output "band_labels.txt"
        Output "band.conf"

        Output "eigenvalues.yaml" -- band.yaml directly from phonopy (no post-processing)
        Output "data.dat"

        Output "sc.conf" -- awkward relic of the past...?

        Output "gauss_spectra.dat"

    enter "sp2/[c]/[x]" $ do

        ephemeralFile "some.log"
        ephemeralFile "log.lammps"

        isolate
            [ Produces "relaxed.vasp" (From "POSCAR")
            -------------------------------------------------
            , Requires   "moire.vasp" (As "moire.vasp")
            , Requires  "config.json" (As "config.json")
            , Records      "some.log" (From "some.log")
            , Records    "log.lammps" (From "log.lammps")
            , KeepOnError
            ] $ \tmpDir _ ->
                loudIO . eggInDir tmpDir $ doMinimization "moire.vasp"

        isolate
            [ Produces    "disp.conf" (From "disp.conf")
            , Produces    "disp.yaml" (From "disp.yaml")
            -------------------------------------------------
            , Requires "relaxed.vasp" (As "moire.vasp")
            , Requires  "config.json" (As "config.json")
            , Records      "some.log" (From "some.log")
            , Records    "log.lammps" (From "log.lammps")
            -------------------------------------------------
            -- we acknowledge the creation of, but don't care much about:
            --     phonopy_disp.yaml
            --     SPOSCAR
            --     POSCAR-[x]
            ] $ \tmpDir _ ->
                loudIO . eggInDir tmpDir $ sp2Displacements

        isolate
            [ Produces           "FORCE_SETS" (From "FORCE_SETS")
            , Produces "force_constants.hdf5" (From "force_constants.hdf5")
            , Produces     "eigenvalues.yaml" (From "band.yaml")
            , Produces      "band_labels.txt" (From "band_labels.txt")
            , Produces            "band.conf" (From "band.conf")
            -------------------------------------------------
            , Requires    "disp.yaml" (As "disp.yaml")
            , Requires "relaxed.vasp" (As "moire.vasp")
            , Requires  "config.json" (As "config.json")
            , Records      "some.log" (From "some.log")
            , Records    "log.lammps" (From "log.lammps")
            -------------------------------------------------
            -- we acknowledge the creation of, but don't care much about:
            --     phonopy.yaml
            ] $ \tmpDir _ ->
                loudIO . eggInDir tmpDir $ sp2Forces

        isolate
            [ Produces    "gauss_spectra.dat" (From "gauss_spectra.dat")
            -------------------------------------------------
            , Requires            "disp.yaml" (As "disp.yaml")
            , Requires           "FORCE_SETS" (As "FORCE_SETS")
            , Requires "force_constants.hdf5" (As "force_constants.hdf5")
            , Requires         "relaxed.vasp" (As "moire.vasp")
            , Requires          "config.json" (As "config.json")
            ] $ \tmpDir _ ->
                loudIO . eggInDir tmpDir $ sp2Raman

        --------------------
        -- a phonopy input file with just the supercell
        "sc.conf" !> \scConf F{..} -> do
            bandConf <- needsFile "band.conf"
            readModifyWrite (filter ("DIM " `isPrefixOf`))
                            (readLines bandConf)
                            (writeLines scConf)

-- Computations that analyze the relationship between VDW and non-VDW
crossAnalysisRules :: App ()
crossAnalysisRules = do

    informalSpec "ev-cache" $ do
        Input "force_constants.hdf5"
        Input "FORCE_SETS"
        Input "sc.conf"
        Input "relaxed.vasp"
        Input "hsym.json"
        ----------------
        Surrogate "init-ev-cache"
        Note "you should explicitly 'need' the surrogate and then use Eigenvectors.withCache"

    enter "ev-cache/[c]/[x]/" $ do
        surrogate "init-ev-cache"
            [] $ "" #> \_ F{..} -> do
                let files = [ ("force_constants.hdf5",  file "force_constants.hdf5")
                            , ("FORCE_SETS",            file "FORCE_SETS")
                            , ("sc.conf",               file "sc.conf")
                            , ("POSCAR",                file "relaxed.vasp")
                            ]
                mapM_ (needs . snd) files

                let density = 100 -- HACK
                qPath <- needsFile "hsym.json" >>= liftIO . readQPathFromHSymJson density

                liftIO $ Eigenvectors.initCache files
                                                ["--readfc", "--hdf5", "sc.conf"]
                                                qPath
                                                (file ".")

    ---------------------------
    -- band uncrossing
    --
    -- this is an expensive, optional step that occurs between
    --   [p]/[v]/eigenvalues-orig.yaml and [p]/[v]/eigenvalues.yaml
    --
    -- it compares the band structures for nonvdw and vdw, and permutes the bands at each kpoint
    --  in an attempt to do two things:
    --
    -- 1. within a band structure:
    --     fix places where bands cross at non-high-symmetry points to obtain continuous curves
    -- 2. between the two structures:
    --     correctly associate each shifted band with its unshifted counterpart
    --
    -- it is not perfect

    informalSpec "uncross" $ do
        Input "hsym.json"
        Input "[v]/eigenvalues.yaml"
        Symlink "[v]/ev-cache"
        ----------------
        Output "[v]/corrected.yaml"

    -- uncomment to ENABLE uncrossing
    enter "uncross/[c]/[x]" $ do
        surrogate "run-uncross"
            [ ("[v]/corrected.yaml", 2)
            ] $ "" #> \_ F{..} -> do

                let density = 100 -- XXX
                qPath   <- needsFile "hsym.json" >>= liftIO . readQPathFromHSymJson density
                esNoVdw <- needsFile "novdw/eigenvalues.yaml" >>= liftIO . Phonopy.readQPathEnergies
                esVdw   <- needsFile   "vdw/eigenvalues.yaml" >>= liftIO . Phonopy.readQPathEnergies
                needSurrogateFile "init-ev-cache" "novdw/ev-cache"
                needSurrogateFile "init-ev-cache" "vdw/ev-cache"
                liftIO $
                    Eigenvectors.withCache (file "novdw/ev-cache") $ \(Just vsNoVdw) ->
                        Eigenvectors.withCache (file "vdw/ev-cache") $ \(Just vsVdw) -> do

                            let cfg = Uncross.UncrossConfig { Uncross.cfgQPath             = qPath
                                                            , Uncross.cfgOriginalEnergiesA = esNoVdw
                                                            , Uncross.cfgOriginalEnergiesB = esVdw
                                                            , Uncross.cfgOriginalVectorsA  = vsNoVdw
                                                            , Uncross.cfgOriginalVectorsB  = vsVdw
                                                            , Uncross.cfgWorkDir = file "work"
                                                            }
                            (permsNoVdw, permsVdw) <- Uncross.runUncross cfg

                            readModifyWrite (Phonopy.permuteBandYaml permsNoVdw)
                                            (readYaml (file "novdw/eigenvalues.yaml"))
                                            (writeYaml (file "novdw/corrected.yaml"))
                            readModifyWrite (Phonopy.permuteBandYaml permsVdw)
                                            (readYaml (file "vdw/eigenvalues.yaml"))
                                            (writeYaml (file "vdw/corrected.yaml"))

    -- -- uncomment to DISABLE uncrossing
    -- enter "uncross/[c]/[x]" $ do
    --     surrogate "run-uncross"
    --         [ ("[v]/corrected.yaml", 2)
    --         ] $ "" #> \_ F{..} -> do
    --             copyPath (file   "vdw/eigenvalues.yaml") (file   "vdw/corrected.yaml")
    --             copyPath (file "novdw/eigenvalues.yaml") (file "novdw/corrected.yaml")

    ----------------------------------------

    -- 1st order perturbation theory

    -- currently does not use uncrossed bands, in part because it currently repeats much of the
    -- heavy number crunching done by the uncrosser, and also because I'd rather have the
    -- uncrosser depend ON this!
    --
    -- (specifically, I imagine that the replacing the novdw eigenkets with the zeroth order
    --  perturbed kets could improve the results of uncrossing immensely, and perhaps allow
    --  some of the hairier bits of logic in the uncrosser to be removed)

    informalSpec "perturb1" $ do
        Input "[v]/eigenvalues.yaml"
        Symlink "[v]/ev-cache"
        ----------------
        Output "vdw/perturb1.yaml"

    enter "perturb1/[c]/[x]" $ do

        "vdw/perturb1.yaml" !> \_ F{..} -> do

            esNoVdw <- needsFile "novdw/eigenvalues.yaml" >>= liftIO . Phonopy.readQPathEnergies
            esVdw   <- needsFile   "vdw/eigenvalues.yaml" >>= liftIO . Phonopy.readQPathEnergies

            needSurrogateFile "init-ev-cache" "novdw/ev-cache"
            needSurrogateFile "init-ev-cache" "vdw/ev-cache"
            liftIO $
                Eigenvectors.withCache (file "novdw/ev-cache") $ \(Just vsNoVdw) ->
                    Eigenvectors.withCache (file "vdw/ev-cache") $ \(Just vsVdw) -> do

                        e1s <- Vector.forM (Vector.fromList (Phonopy.qPathAllIds esNoVdw)) $ \q -> liftIO $ do
                            let unperturbedEs = esNoVdw `Phonopy.qPathAt` q
                            let exactEs       = esVdw   `Phonopy.qPathAt` q
                            unperturbedVs <- vsNoVdw `Phonopy.qPathAt` q
                            exactVs       <- vsVdw   `Phonopy.qPathAt` q

                            let (perturbedEs, _) = Uncross.firstOrderPerturb 0 (unperturbedEs, unperturbedVs)
                                                                                (exactEs,       exactVs)
                            pure perturbedEs

                        readModifyWrite (Phonopy.putBandYamlSpectrum e1s)
                                        (readYaml  (file "novdw/eigenvalues.yaml"))
                                        (writeYaml (file "perturb1.yaml"))

    ----------------------------------------

    informalSpec "fold" $ do
        Input "template.yaml" -- ANY band.yaml from the superstructure
        Input "coeffs.json"
        Input "hsym.json"
        Input "sub/structure.vasp"
        Input "sub/force_constants.hdf5"
        Input "sub/sc.conf"
        ----------------
        Output "out.yaml"

    -- band folding
    enter "fold/[c]/[x]" $ do
        "out.yaml" !> \outYaml F{..} -> do

            cMat <- needJSONFile "coeffs.json"
            let unitCompute   :: [[Double]] -> Act [[Double]]
                foldedCompute :: [[Double]] -> Act [[Double]]
                unitCompute = computeStructureBands (file "sub/structure.vasp")
                                                    (file "sub/force_constants.hdf5")
                                                    (file "sub/sc.conf")
                foldedCompute = fmap (fmap sort) . foldBandComputation cMat unitCompute

            let pointDensity = 100 -- FIXME
            qs <- needsFile "hsym.json"
                    >>= liftIO
                        . fmap (fmap toList) . fmap toList
                        . readQPathFromHSymJson pointDensity

            -- write the unfolded band structure into a valid band.yaml
            es <- foldedCompute qs
            -- TODO validate number of bands
            readModifyWrite (Phonopy.putBandYamlSpectrum (Vector.fromList . fmap Vector.fromList $ es))
                            (needsFile "template.yaml" >>= liftIO . readYaml)
                            (liftIO . writeYaml outYaml)

    informalSpec "unfold" $ do
        Input "template.yaml" -- ANY band.yaml from the superstructure
        Input "coeffs.json"
        Input "hsym.json"
        Input "super/structure.vasp"
        Input "super/force_constants.hdf5"
        Input "super/sc.conf"
        ----------------
        Output "out.yaml"

    -- band unfolding
    enter "unfold/[c]/[x]" $ do
        "out.yaml" !> \outYaml F{..} -> do
            -- NOTE: yes, it bothers me too that this basically looks like the output
            --        of 'sed' on the "fold" code...

            cMat <- needJSONFile "coeffs.json"
            let thisCompute     :: [[Double]] -> Act [[Double]]
                unfoldedCompute :: [[Double]] -> Act [[Double]]
                thisCompute = computeStructureBands (file "super/structure.vasp")
                                                    (file "super/force_constants.hdf5")
                                                    (file "super/sc.conf")
                unfoldedCompute = fmap (fmap sort) . unfoldBandComputation cMat thisCompute

            let pointDensity = 100 -- FIXME
            qs <- needsFile "hsym.json"
                    >>= liftIO
                        . fmap (fmap toList) . fmap toList
                        . readQPathFromHSymJson pointDensity

            -- write the unfolded band structure into a valid band.yaml
            es <- unfoldedCompute qs
            -- TODO validate number of bands
            readModifyWrite (Phonopy.putBandYamlSpectrum (Vector.fromList . fmap Vector.fromList $ es))
                            (needsFile "template.yaml" >>= liftIO . readYaml)
                            (liftIO . writeYaml outYaml)

mainRules :: App ()
mainRules = do

    let perfectABPattern = "001-b"

    ------------------------------
    -- The work/ subtree is where we put everything together.
    -- This is where we define which instances actually exist of the various components,
    --  and hook up all of their inputs and outputs.
    --
    -- First, we define instances of the component computations by creating symlinks.
    -- (this MUST be done first)

    "work/[p]/pat"          `isDirectorySymlinkTo` "input/pat/[p]"

    -- we'll use "pat" for any instance that is not otherwise worth naming
    "work/[p]/assemble"     `isDirectorySymlinkTo` "assemble/pat/[p]"
    "work/[p]/ev-cache.[v]" `isDirectorySymlinkTo` "ev-cache/pat/[p].[v]"
    "work/[p]/sp2.[v]"            `isDirectorySymlinkTo`  "sp2/pat/[p].[v]"
    "work/[p]/perfect-ab-sp2.[v]" `isDirectorySymlinkTo` ("sp2/pat/" ++ perfectABPattern ++ ".[v]") -- for folding
    "work/[p]/uncross"      `isDirectorySymlinkTo` "uncross/pat/[p]"
    "work/[p]/perturb1"     `isDirectorySymlinkTo` "perturb1/pat/[p]"
    "work/[p]/fold.[v]"     `isDirectorySymlinkTo` "fold/pat/[p].[v]"
    "work/[p]/unfold.[v]"   `isDirectorySymlinkTo` "unfold/pat/[p].[v]"
    "work/[p]/bandplot"     `isDirectorySymlinkTo` "bandplot/[p]"
    "uncross/pat/[p]/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/pat/[p].[v]/"
    "perturb1/pat/[p]/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/pat/[p].[v]/"

    "work/[p]/hsym.json" `isCopiedFromFile` "input/hsym.json"

    ------------------------------
    -- Now, we can hook up all of the input and output files.
    -- We can freely use our symlinks; there are mechanisms built into our 'App' monad
    --  which ensure that the dependency tree we present to Shake has the correct structure.

    enter "work/[p]" $ do

        -- HACK: focus on ab patterns for quickest possible route to restore working order
        "positions.json"               `isHardLinkToFile` "pat/ab/positions.json"
        "assemble/spatial-params.toml" `isCopiedFromFile` "pat/ab/spatial-params.toml"
        "assemble/layers.toml"         `isCopiedFromFile` "pat/ab/layers.toml"

        let configRule lj = \path F{..} -> do
            copyPath (file "pat/ab/input/config.json") path
            loudIO $ setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj
        "sp2.vdw/config.json"   !> configRule True
        "sp2.novdw/config.json" !> configRule False
        "sp2.[v]/moire.vasp" `isHardLinkToFile` "assemble/moire.vasp"

        "ev-cache.[v]/force_constants.hdf5" `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/FORCE_SETS"           `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/sc.conf"              `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/relaxed.vasp"         `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/hsym.json"            `isCopiedFromFile` "hsym.json"
        "uncross/hsym.json"             `isCopiedFromFile` "hsym.json"
        "uncross/[v]/eigenvalues.yaml"  `isHardLinkToFile` "sp2.[v]/eigenvalues.yaml"
        "perturb1/[v]/eigenvalues.yaml" `isHardLinkToFile` "sp2.[v]/eigenvalues.yaml"

        "fold.[v]/template.yaml"            `isHardLinkToFile` "sp2.[v]/eigenvalues.yaml"
        "fold.[v]/coeffs.json"              `isCopiedFromFile` "coeffs.json"
        "fold.[v]/hsym.json"                `isCopiedFromFile` "hsym.json"
        "fold.[v]/sub/structure.vasp"       `isHardLinkToFile` "perfect-ab-sp2.[v]/relaxed.vasp"
        "fold.[v]/sub/force_constants.hdf5" `isHardLinkToFile` "perfect-ab-sp2.[v]/force_constants.hdf5"
        "fold.[v]/sub/sc.conf"              `isCopiedFromFile` "perfect-ab-sp2.[v]/sc.conf"

        "unfold.[v]/template.yaml"              `isHardLinkToFile` "sp2.[v]/eigenvalues.yaml"
        "unfold.[v]/coeffs.json"                `isCopiedFromFile` "coeffs.json"
        "unfold.[v]/hsym.json"                  `isCopiedFromFile` "hsym.json"
        "unfold.[v]/super/structure.vasp"       `isHardLinkToFile` "sp2.[v]/relaxed.vasp"
        "unfold.[v]/super/force_constants.hdf5" `isHardLinkToFile` "sp2.[v]/force_constants.hdf5"
        "unfold.[v]/super/sc.conf"              `isCopiedFromFile` "sp2.[v]/sc.conf"

        "bandplot/input-data-vdw.dat"             `datIsConvertedFromYaml` "uncross/vdw/corrected.yaml"
        "bandplot/input-data-novdw.dat"           `datIsConvertedFromYaml` "uncross/novdw/corrected.yaml"
        "bandplot/input-data-[v]-folded-ab.dat"   `datIsConvertedFromYaml` "fold.[v]/out.yaml"
        "bandplot/input-data-[v]-unfolded-ab.dat" `datIsConvertedFromYaml` "unfold.[v]/out.yaml"
        "bandplot/input-data-[v]-perfect-ab.dat"  `datIsConvertedFromYaml` "perfect-ab-sp2.[v]/eigenvalues.yaml"
        "bandplot/input-data-perturb1.dat"        `datIsConvertedFromYaml` "perturb1/perturb1.yaml"

    -- a final couple of awkward bits and pieces of data needed by 'bandplot/'
    enter "work/[p]" $ do
        "coeffs.json" %> \F{..} -> do
            result <- maybe undefined id <$> needJSONFile "positions.json"
            mat <- either fail pure . flip Aeson.parseEither result $
                (Aeson..: "meta") >=> (Aeson..: "coeff") >=> (aesonIndex 0)
            pure (mat :: [[Int]])

        "bandplot/title" !> \title F{..} ->
                readModifyWrite head (readLines (file "sp2.novdw/moire.vasp"))
                                    (writePath title)

        "band_labels.txt" `isCopiedFromFile` "sp2.novdw/band_labels.txt"
        "bandplot/band_xticks.txt" !> \xvalsTxt F{..} -> do
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

        -- files created by datIsConvertedFromYaml` have a short prelude containing xtick positions
        "known-to-have-a-prelude.dat" `isHardLinkToFile` "bandplot/input-data-vdw.dat"
        "data-prelude.dat" !> \prelude F{..} ->
                readModifyWrite (take 3) (readLines (file "known-to-have-a-prelude.dat"))
                                         (writeLines prelude)

plottingRules :: App ()
plottingRules = do
    -- HACK
    -- some parts of the plotting process look at a couple of pieces of sp2 output
    --  and I'm not yet up to untangling them.

    "bandplot/[p]/band_labels.txt" `isCopiedFromFile` "sp2/pat/[p].novdw/band_labels.txt"

    "bandplot/[p]/prelude.gplot"          `isCopiedFromFile` "input/gplot-helper/prelude.gplot"
    "bandplot/[p]/write-band-[ext].gplot" `isCopiedFromFile` "input/gplot-helper/write-band-[ext].gplot"

    "bandplot/[p]/templates" `isDirectorySymlinkTo` "input/gplot-templates"

    enter "bandplot" $ do

        enter "[p]" $ do

            -- FIXME there's a very messy story here with .json and .dat formats
            --
            -- In summary:
            --   - Earlier code primarily took JSON files as input because they were easier to parse,
            --     ...but produced .dat files, in order I think to avoid clashing with a blanket rule for .json.
            --
            --   - Some code is now forced to work with DAT files directly as input (with awful ad-hoc parsing logic)
            --     because filtering the data produces irregularly-shaped data that my JSON <-> DAT conversion
            --     utilities cannot handle, because they were designed with features in mind such as permuting
            --     the axes (which isn't well-defined in general for jagged data; I think you can only take
            --               regularly shaped axes and lift them upwards, a la Distribute)

            -- For producing a .dat file from a .json VALUE
            let produceDat fp cols =
                    let !(Just _) = onlyUniqueValue (length <$> cols) in
                    liftAction $ engnuplot (Stdin (idgaf $ Aeson.encode cols)) (FileStdout fp)

            -- NOTE:  JSON -> .dat
            let extractColumn i fp = \dataOut F{..} -> do
                [cols] <- needDataJson [file fp]
                produceDat dataOut [cols !! 0, cols !! i]

            -- Filter data to just shifted bands, based on the difference between a novdw column and a vdw column.
            -- This produces jagged data unsuitable for analysis (only suitable for display),
            --  so only do it as the final step.
            -- NOTE:  JSON -> .dat (and this one CANNOT make a .json)
            let filterShiftedOnColumns i1 i2 fp =
                    -- (the implementation is actually so obnoxiously large that I've relocated it.
                    --   This stub is kept around for the sake of labeling input/output formats)
                    filterShiftedOnColumnsImpl i1 i2 fp

            let writeText      :: FileString ->  Text  -> Act ()
                writeTextLines :: FileString -> [Text] -> Act ()
                writeText dest = liftIO . Text.writeFile dest
                writeTextLines dest = writeText dest . Text.unlines

            let needText      :: Pat -> Act  Text
                needTextLines :: Pat -> Act [Text]
                needText = needs >=> liftIO . Text.readFile
                needTextLines = fmap Text.lines . needText

            -- Extract a column while bypassing my JSON utils, so that we can work on irregular arrays.
            -- NOTE:  .dat -> .dat
            let extractColumnDirectly i fp = \dataOut F{..} -> readModifyWrite (fmap f)
                                                                               (needTextLines (file fp))
                                                                               (writeTextLines dataOut)
                  where f "" = ""
                        f s | "#" `Text.isPrefixOf` s = s
                            | otherwise = let w = Text.words s in
                                          Text.unwords [w !! 0, w !! i]

            -- Multiplex multiple possibly-jagged sources into a single file
            --  by putting labels in the very first column to help distinguish
            --  which data series belongs to which file.
            -- They files are joined by double-blanks.
            --
            -- This exists so that we can still produce a single .dat file
            --  even if we want to plot multiple sets of jagged data.
            -- (the reason for this limitation being the blanket rule on plotting)
            --
            -- NOTE:  [(String, .dat)] -> .dat
            let multiplexDirectly sources dataOut F{..} = doStuff
                    where
                      doStuff = do
                          linesBySource <- (mapM getLinesForSource sources) :: Act [[Text]]
                          liftIO . Text.writeFile dataOut . Text.intercalate "\n\n" . fmap Text.unlines $ linesBySource

                      getLinesForSource :: (Text, Pat) -> Act [Text]
                      getLinesForSource (label, fp) = decorateLines label <$> needTextLines (file fp)

                      decorateLines :: Text -> [Text] -> [Text]
                      decorateLines label = fmap $ \case "" -> ""
                                                         s  -> label <> " " <> s

            "work-data-both.dat" !> \dataOut F{..} -> do
                [[xs, ysN]] <- needDataJson [file "input-data-novdw.json"]
                [[_,  ysV]] <- needDataJson [file "input-data-vdw.json"]
                produceDat dataOut [xs, ysN, ysV]

            "work-data-all.dat" !> \dataOut F{..} -> do
                [[xs, ys0]] <- needDataJson [file "input-data-novdw.json"]
                [[_,  ysV]] <- needDataJson [file "input-data-vdw.json"]
                [[_,  ys1]] <- needDataJson [file "input-data-perturb1.json"]
                produceDat dataOut [xs, ys0, ysV, ys1]

            "work-data-[v]-folded-ab.dat" !> \dataOut F{..} -> do
                [[xs, ys0]] <- needDataJson [file "input-data-[v].json"]
                [[_,  ys1]] <- needDataJson [file "input-data-[v]-folded-ab.json"]
                produceDat dataOut [xs, ys0, ys1]

            "work-data-[v]-unfolded-ab.dat" !> \dataOut F{..} -> do
                [[ _, ys0']] <- needDataJson [file "input-data-[v]-perfect-ab.json"]
                [[xs, ys1 ]] <- needDataJson [file "input-data-[v]-unfolded-ab.json"]
                numDupes <- patternVolume (fmt "[p]")
                let ys0 = ffmap (>>= Vector.replicate numDupes) ys0'

                produceDat dataOut [xs, ys0, ys1]

            "work-data-filter.dat" !> filterShiftedOnColumns 1 2 "work-data-both.json"
            "work-data-filter-just-novdw.dat" !> extractColumnDirectly 1 "work-data-filter.dat"
            "work-data-filter-just-vdw.dat"   !> extractColumnDirectly 2 "work-data-filter.dat"

            -- Jsonification
            "[x]-data-[y].json" !> \json F{..} -> do
                dat <- needsFile "[x]-data-[y].dat"
                liftAction $ degnuplot (FileStdin dat) (FileStdout json)

            --------------------------
            -- plot-data files
            -- There should be one for each .gplot.template rule
            -- FIXME these should perhaps be defined nearer those!

            "plot-data-novdw.dat"           `isHardLinkToFile` "input-data-novdw.dat"
            "plot-data-vdw.dat"             `isHardLinkToFile` "input-data-vdw.dat"
            "plot-data-both.dat"            `isHardLinkToFile` "work-data-both.dat"
            "plot-data-perturb1.dat"        `isHardLinkToFile` "work-data-all.dat"
            "plot-data-filter.dat"          `isHardLinkToFile` "work-data-filter.dat"
            "plot-data-[v]-folded-ab.dat"   `isHardLinkToFile` "work-data-[v]-folded-ab.dat"
            "plot-data-[v]-unfolded-ab.dat" `isHardLinkToFile` "work-data-[v]-unfolded-ab.dat"

            "plot-data-[v]-folded-ab-filter.dat" !>
                multiplexDirectly [ ("NaturalFilt", "work-data-filter-just-[v].dat")
                                  , ("NaturalAll",  "input-data-[v].dat")
                                  , ("Folded",      "input-data-[v]-folded-ab.dat")
                                  ]

            -- individual bands, for gauging the quality of the uncrosser
            "plot-data-num-[i].dat" !> \dataOut F{..} -> do
                (xs, ysN) <- needJSONFile "input-data-novdw.json" :: Act ([[[Double]]], [[[Double]]])
                (_,  ysV) <- needJSONFile "input-data-vdw.json"   :: Act ([[[Double]]], [[[Double]]])
                let extractBandI = fmap (List.transpose . (:[]) . (!! read (fmt "[i]")) . List.transpose)
                produceDat dataOut $ extractBandI <$> [xs, ysN, ysV]

    enter "bandplot" $ do
        enter "[p]" $ do
            "xbase.gplot" !> \xbase F{..} -> do
                    title <- needsFile "title" >>= readPath
                    xticksLine <- needsFile "band_xticks.txt" >>= readPath

                    let dquote = \s -> "\"" ++ s ++ "\""
                    writeLines xbase
                        [ "set title " ++ dquote (idgaf title)
                        , xticksLine
                        , "band_n = 3" -- FIXME: use hsym.json
                        -- (now that we have a solution for temp dirs, we can just
                        --  set this string to a constant)
                        , "data = " ++ dquote "data.dat"
                        ]

    enter "bandplot" $ do
        enter "[p]" $ do
            "vdw.gplot.template"                  `isCopiedFromFile` "templates/band.gplot.template"
            "novdw.gplot.template"                `isCopiedFromFile` "templates/band.gplot.template"
            "both.gplot.template"                 `isCopiedFromFile` "templates/both.gplot.template"
            "filter.gplot.template"               `isCopiedFromFile` "templates/filter.gplot.template"
            "[v]-folded-ab.gplot.template"        `isCopiedFromFile` "templates/folded.gplot.template"
            "[v]-unfolded-ab.gplot.template"      `isCopiedFromFile` "templates/folded.gplot.template"
            "[v]-folded-ab-filter.gplot.template" `isCopiedFromFile` "templates/folded-filter.gplot.template"
            "perturb1.gplot.template"             `isCopiedFromFile` "templates/perturb1.gplot.template"
            "num-[i].gplot.template"              `isCopiedFromFile` "templates/both.gplot.template"

    enter "bandplot" $
        enter "[p]" $ do

            isolate
                [ Produces "plot_[s].[x]"         (From "band.out")
                -----------------------------------------------------
                , Requires "prelude.gplot"        (As "prelude.gplot") -- init and math funcs
                , Requires "xbase.gplot"          (As "xbase.gplot")   -- some vars from data
                , Requires "[s].gplot.template"   (As "gnu.gplot")     -- the actual plot
                , Requires "write-band-[x].gplot" (As "write.gplot")   -- term and file output
                , Requires "plot-data-[s].dat"    (As "data.dat")
                ] $ \tmpDir fmt -> do
                    () <- liftAction $
                        -- turns out we don't need to put it all in one file
                        cmd "gnuplot prelude.gplot xbase.gplot gnu.gplot write.gplot" (Cwd tmpDir)
                    moveUntracked (fmt (tmpDir </> "band.[x]")) (tmpDir </> "band.out") -- FIXME dumb hack

    -- "out/" for collecting output across all patterns
    liftIO $ createDirectoryIfMissing True "out/bands"
    "out/bands/[p]_[s].[ext]" `isCopiedFromFile` "bandplot/[p]/plot_[s].[ext]"

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

readQPathFromHSymJson :: Int -> FileString -> IO _
readQPathFromHSymJson density fp = do
    hSymPath <- readJSON fp :: IO Phonopy.HighSymInfo
    pure $ Phonopy.phonopyQPath (Phonopy.highSymInfoQPoints hSymPath)
                                (replicate (Phonopy.highSymInfoNLines hSymPath) density)

dataJsonRule :: Pat -> (Fmts -> Act [BandGplColumn]) -> App ()
needDataJson :: [FileString] -> Act [[BandGplColumn]]
SerdeFuncs { sfRule=dataJsonRule
           , sfNeed=needDataJson
           } = serdeFuncs :: SerdeFuncs [BandGplColumn]

-- Make a pair of `!>` and `need` functions that work with a JSON serialized datatype.
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

-- Helper for using temporary directories to isolate actions that have side-effects.
-- (due to e.g. a command that uses fixed filenames)
--
-- It produces a Rule, which allows the output files to be specified in a single
--  location (otherwise, they end up needing to appear once as rule outputs, and
--  again as a list of files to be copied out)
--
-- The continuation receives:
--  - the temp dir filepath
--  - the "fmt" formatter (i.e. the one which doesn't append the prefix)
-- and is expected to produce a family of files (i.e. satisfying the conditions
-- described in the documentation for 'family'.)
--
-- cwd is not changed.
--
-- NOTE: mind that any "xxxFile" functions will continue to use the entered
--       directory rather than the temp directory.
--       This is probably not what you want.
isolate :: [TempDirItem] -> (FileString -> Fmt -> Act ()) -> App ()
isolate items act = result
  where
    result = family (items >>= producedPats) &!> \_ F{..} ->
        myWithTempDir $ \tmp -> do

            forM_ items $ \case
                Requires treePat (As tmpPath) -> copyPath (file treePat) (tmp </> tmpPath)
                _                             -> pure ()

            act tmp fmt

            -- check file existence ahead of time to avoid producing an incomplete set of output files
            whenJustM (findM (liftIO . fmap not . doesFileExist) (items >>= outputSources tmp))
                      (\a -> error $ "isolate: failed to produce file: " ++ a)

            forM_ items $ \case
                Produces treePat (From tmpPath) -> askFile treePat >>= copyUntracked (tmp </> tmpPath)
                Records  _       _              -> pure () -- XXX we don't have an appendPath function yet
                _                               -> pure ()

    myWithTempDir = if any (KeepOnError ==) items then withTempDirDebug
                                                  else withTempDir

    outputSources tmp (Produces _ (From tmpPath)) = [tmp </> tmpPath]
    outputSources _ _ = []
    producedPats (Produces treePat _) = [treePat]
    producedPats _                    = []

-- these newtypes just help clarify the roles of some args so that they
-- aren't accidentally transposed...
newtype As a   = As   a deriving (Eq, Ord, Show, Read)
newtype From a = From a deriving (Eq, Ord, Show, Read)

data TempDirItem
    = Requires   Pat (As   FileString) -- copy an input file as a tracked dependency
    | Produces   Pat (From FileString) -- copy an output file
    | Records    Pat (From FileString) -- append contents of a log file
    | KeepOnError                    -- keep temp dir for debugging on error
    deriving (Eq, Show, Read, Ord)

------------------------------------------------------------

-- Operator to create a band.yaml -> data.dat rule.
datIsConvertedFromYaml :: Pat -> Pat -> App ()
datIsConvertedFromYaml dataPat yamlPat =
    isolate
        [ Produces dataPat (From "out.dat")
        , Requires yamlPat (As "band.yaml")
        ] $ \tmp _ ->
            liftAction $ cmd "bandplot --gnuplot" (Cwd tmp) (FileStdout (tmp </> "out.dat"))

-- Ask phonopy for eigenvalues, using a temp dir to preserve our sanity.
-- NOTE: Doesn't use 'isolate' because it isn't a rule
computeStructureBands :: FileString -- POSCAR
                      -> FileString -- force_constants.hdf5
                      -> FileString -- configFile
                      -> [[Double]] -- qpoints (fractional recip. space)
                      -> Act [[Double]] -- ascending frequncies (THz) at each qpoint
computeStructureBands fpPoscar fpForceConstants fpConf qs =
    withTempDirDebug $ \tmp -> do
        linkPath fpPoscar         (tmp </> "POSCAR")
        linkPath fpForceConstants (tmp </> "force_constants.hdf5")
        copyPath fpConf           (tmp </> "band.conf")

        let qs3d = fmap (take 3 . (++ repeat 0)) qs ++ [[0,0,0]]
        liftIO $ readFile (tmp </> "band.conf") >>= traceIO . idgaf
        appendLines (tmp </> "band.conf")
            [ "BAND_POINTS = 1"
            , "BAND = " ++ List.unwords (show <$> concat qs3d)
            ]

        () <- liftAction $ cmd "phonopy --hdf5 --readfc band.conf" (Cwd tmp)


        -- _ <- liftIO $ fmap toList . toList . Phonopy.getBandYamlSpectrum <$> readYaml (tmp </> "band.yaml")
        -- fail "x_x"
        liftIO $ fmap toList . toList . Phonopy.getBandYamlSpectrum <$> readYaml (tmp </> "band.yaml")

-----------------------------

-- relocated implementation of a function for dealing with plot data
filterShiftedOnColumnsImpl :: _ -- deal with it, ghc
filterShiftedOnColumnsImpl i1 i2 fp = \dataOut F{..} -> do

    [cols] <- needDataJson [file fp]

    -- we're going to want to put things in .dat order for this.
    -- ...never mind the irony that the original data.dat was in this order,
    --    until we shuffled it around for the json file.
    let fromJust = maybe undefined id
    let transpose' :: (MonadZip m, Traversable t)=> t (m a) -> m (t a)
        transpose' = fromJust . unwrapZip . fmap (fromJust . unwrapZip) . sequenceA . fmap wrapZip . wrapZip

    let permuteTheAxes :: [VVVector Double] -> VVVector [Double]
        permuteTheAxes = -- Despite appearances, this is not a modern art exhibit.
                            -- This sequence of transposes replicates the behavior of
                            --   the "-Gbhkx -Jxhkb" flags to engnuplot.
                        id                    --  x   h   k   b  (xy-var, highsym, kpoint, band) INITIAL
                        >>>       transpose'  --  h . x   k   b
                        >>>  fmap transpose'  --  h   k . x   b
                        >>> ffmap transpose'  --  h   k   b . x
                        >>>  fmap transpose'  --  h   b . k   x
                        >>>       transpose'  --  b . h   k   x  (band, highsym, kpoint, xy-var) FINAL

    let goodRow xs = (log . abs) ((xs !! i1) - (xs !! i2)) >= -4
    let runsSatisfying p = rec where
            rec v | null v            = []
                    | p (Vector.head v) = let (run,v') = Vector.span p v        in run : rec v'
                    | otherwise         = let v' = Vector.dropWhile (not . p) v in       rec v'

    let transform :: VVVector [Double] -> VVVector [Double]
        transform =
                    id                                      -- Type:  V V V V Double
                    -- filter out unwanted entries...
                    >>> ffmap (runsSatisfying goodRow)      -- Type:  V V [] V V Double
                    -- treat discontinuities due to filtering
                    -- with a single blank line, same as highsym lines.
                    -- (we merge the list up one level)
                    >>> fmap (>>= Vector.fromList)          -- Type:  V  V   V V Double

    -- Don't use my gnuplot tools since they currently expect uniformly shaped arrays. (oops)
    -- This is the easy part, anyways. (Serialization)
    let serialize :: VVVector [Double] -> Text
        serialize =   ffffmap (idgaf . show)
                    >>>  fffmap Text.unwords
                    >>>   ffmap (Text.unlines . toList)
                    >>>    fmap (Text.intercalate "\n" . toList)
                    >>>         (Text.intercalate "\n\n" . toList)

    liftIO $
        cols & (permuteTheAxes >>> transform >>> serialize >>> writeFile dataOut)

------------------------------------------------------------

moveUntracked :: (MonadIO io)=> FileString -> FileString -> io ()
moveUntracked = mv `on` idgaf
copyUntracked :: (MonadIO io)=> FileString -> FileString -> io ()
copyUntracked = cp `on` idgaf

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

-- FIXME I wanted to read these from JSON now that we're out of the bash tarpit.
--       (In fact, dynamically-generated dependencies is Shake's specialty!)
kpointShorts :: [[Char]]
kpointShorts = ["g", "m", "k"]
kpointLoc :: (Fractional t, IsString a, Eq a) => a -> [t]
kpointLoc "g" = [0, 0, 0]
kpointLoc "m" = [1/2, 0, 0]
kpointLoc "k" = [1/3, 1/3, 0]
kpointLoc _   = error "bugger off ghc"


-- an analogue to Aeson..: for indexing arrays
aesonIndex :: (Aeson.FromJSON x)=> Int -> Aeson.Array -> Aeson.Parser x
aesonIndex i = Aeson.parseJSON . (Vector.! i)

-- HACK: tied to input file structure
-- HACK: shouldn't assume ab
patternVolume :: FileString -> Act Int
patternVolume p = do
    result <- maybe undefined id <$> needJSON ("input/pat" </> p </> "ab/positions.json")
    pure . maybe undefined id . flip Aeson.parseMaybe result $
        (Aeson..: "meta") >=> (Aeson..: "volume") >=> (aesonIndex 0)


--    "//irreps-*.yaml" !> \dest [stem, kpoint] -> runPhonopyIrreps stem kpoint dest

-- common pattern to perform an egg in a directory
eggInDir :: (_)=> s -> Egg a -> Egg a
eggInDir s e = singularToEgg $ pushd (idgaf s) >>= \() -> liftEgg e

script :: FileString -> PartialCmd
script x = cmd ("scripts" </> x)

------------------------------------------------------

doMinimization :: FilePath -> Egg ()
doMinimization original = do
                             -- NOTE: Lattice parameter minimization is currently disabled because
                             --  it took forever on some structures and wrecked them in the process.
                             --
                             -- NOTE: This is for all structures!  Not just AA.
                             -- Also it is plain bizzare.  Relaxing the lattice parameter produces
                             --  structures which are *objectively better* (lower energy),
                             --  but have worse band structure! (negative frequencies)
                             -- This happens even for AB, non-vdw!
                             init
                             ref <- liftIO (newIORef 1.0)
                             _ <- goldenSearch (<) (objective ref)
                                               -- enable lattice param minimization:
                                               -- (1e-3) (0.975,1.036)
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
        echo $ "================================"
        echo $ "BEGIN AT s = " <> repr scale
        echo $ "================================"

        -- ref stores prev scale, since sp2 normalizes scale to 1
        prevScale <- liftIO $ readIORef ref
        liftIO $ writeIORef ref scale

        -- reuse relaxed as guess
        liftIO $ Text.readFile "POSCAR"
                >>= pure . Poscar.fromText
                >>= pure . (\p -> p {Poscar.scale = Poscar.Scale (scale / prevScale)})
                >>= pure . Poscar.toText
                >>= Text.writeFile "POSCAR"

        infos <- fold Fold.list $ killOut $ thisMany 2 $ liftEgg sp2Relax

        echo $ "================================"
        echo $ "RELAXATION SUMMARY AT s = " <> repr scale
        egg $ do
            (i, RelaxInfo energies) <- select (zip [1..] infos)
            echo $ format (" Trial "%d%": "%d%" iterations, V = "%f)
                            i (length energies) (last energies)
        echo $ "================================"
        pure $ last . last . fmap (\(RelaxInfo x) -> x) $ infos

type GsState x y = (x, x, y)
goldenSearch :: forall m y. (Monad m)
             => (y -> y -> Bool)
             -> (Double -> m y)
             -> Double
             -> (Double, Double)
             -> m (Double, y)
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
    init :: Double -> Double -> m (GsState Double _)
    init a d = ((,,) a d) <$> f (getb a d)

    step :: (GsState Double _) -> m (Either (Double,_) (m (GsState Double _)))
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
sp2Relax = (setPhonopyState False False False False False 1000 >>) . liftEgg $ do
    (out,_) <- addStrictOut sp2
    pure $ parseRelaxInfoFromSp2 out

sp2Displacements :: Egg ()
sp2Displacements = setPhonopyState True False False False False 1 >> sp2

sp2Forces :: Egg ()
sp2Forces = setPhonopyState False True True False False 1 >> sp2

sp2Raman :: Egg ()
sp2Raman = setPhonopyState False False False True False 1 >> sp2

setPhonopyState :: (_)=> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> io ()
setPhonopyState d f b r i c = liftIO $ do
    setJson "config.json" ["phonopy", "calc_displacements"] $ Aeson.Bool d
    setJson "config.json" ["phonopy", "calc_force_sets"] $ Aeson.Bool f
    setJson "config.json" ["phonopy", "calc_bands"] $ Aeson.Bool b
    setJson "config.json" ["phonopy", "calc_raman"] $ Aeson.Bool r
    setJson "config.json" ["phonopy", "calc_irreps"] $ Aeson.Bool i
    setJson "config.json" ["relax_count"] $ Aeson.Number (fromIntegral c)

parseRelaxInfoFromSp2 :: Text -> RelaxInfo
parseRelaxInfoFromSp2 = RelaxInfo . mapMaybe iterationEnergy . Text.lines
  where
    mapMaybe f = map f >>> filter isJust >>> sequence >>> maybe undefined id
    -- sp2's output is pretty varied so we use a very specific (and therefore fragile) filter
    iterationEnergy s | "    Value:" `Text.isPrefixOf` s = s & Text.words & (!! 1) & Text.Read.double & either error fst & Just
                      |        " i:" `Text.isPrefixOf` s = s & Text.words & (!! 3) & Text.Read.double & either error fst & Just
                      | otherwise = Nothing

---------------------------------
-- argument parsing for touch rules

appFromArgs :: [()] -> [String] -> IO (Maybe (App ()))
appFromArgs _ args = do

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
    ks <- pure kpointShorts

    let mapMaybe f xs = f <$> xs >>= maybe [] pure
    let Identity func = (iDontCare . Subst.compile) "[p]:::[v]:::[k]"
                        >>= (iDontCare . flip Subst.substIntoFunc pat)

    let pvks = List.intercalate ":::" <$> sequence [ps,vs,ks]
    -- Shake will do the files in arbitrary order if we need them all
    -- at once which sucks because it is nice to do lowest volume first.
    -- need $ orderPreservingUnique (mapMaybe func pvks)
    mapM_ needs $ orderPreservingUnique (mapMaybe func pvks)

all_TouchVer :: Pat -> App ()
all_TouchVer pat = do
    ps <- liftIO allPatterns
    vs <- pure ["vdw", "novdw"]
    ks <- pure kpointShorts

    let mapMaybe f xs = f <$> xs >>= maybe [] pure
    let Identity func = (iDontCare . Subst.compile) "[p]:::[v]:::[k]"
                        >>= (iDontCare . flip Subst.substIntoFunc pat)

    let pvks = List.intercalate ":::" <$> sequence [ps,vs,ks]
    want $ orderPreservingUnique (mapMaybe func pvks)

---------------------------------

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

-- | @cp -a src/* dest@.  "Dumb" because it will fail if the trees have any
--   similar substructure (i.e. src\/subdir and dest\/subdir)
mergetreeDumbUntracked :: (_)=> FileString -> FileString -> Act ()
mergetreeDumbUntracked src dest =
    liftIO (listDirectory src) >>=
        mapM_ (\entry -> cptreeUntracked (src </> entry) (dest </> entry))

-- | @cp -a src dest@
cptreeUntracked :: (_)=> FileString -> FileString -> Act ()
cptreeUntracked src dest = liftAction $ cmd "cp" ["-a", src, dest] (Traced "")
