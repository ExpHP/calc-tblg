#!/usr/bin/env stack

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

module Rules.Comp(
    componentRules,
    GoldenSearchConfig(..),
    ) where


import           ExpHPrelude hiding (FilePath, interact)
import           "base" Data.IORef
import           "base" Data.Function(fix)
import qualified "base" Data.List as List
import qualified "base" Data.Complex as Complex
import           "filepath" System.FilePath.Posix((</>))
import           "extra" Control.Monad.Extra(whenJust)
import qualified "time" Data.Time as Time
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.IO as Text
import qualified "text" Data.Text.Read as Text.Read
import qualified "containers" Data.Map as Map
import qualified "foldl" Control.Foldl as Fold
import qualified "vector" Data.Vector as Vector
import qualified "aeson" Data.Aeson as Aeson
import qualified "vasp-poscar" Data.Vasp.Poscar as Poscar
import           "turtle-eggshell" Eggshell hiding (need,view,empty,(</>))

-- import qualified Turtle.Please as Turtle hiding (empty)
import           JsonUtil
import           ShakeUtil
import qualified Band as Uncross
import qualified Phonopy.Types as Phonopy
import qualified Phonopy.IO as Phonopy
import qualified Phonopy.EigenvectorCache as Eigenvectors
import           Band.Fold(foldBandComputation, unfoldBandComputation)

-- NOTE: these data types are used for automatic serialization.
--       layout matters!
-- data for gnuplot, indexed by:   band, hsymline, kpoint
type VVVector a = Vector (Vector (Vector a))
type BandGplData a = VVVector a
type BandGplColumn = BandGplData Double

-------------------------------------------

componentRules :: App ()
componentRules = do

    informalSpec "input" $ do
        Output "hsym.json"
        Subdir "pat/[p]/[a]" $ do
            Output "spatial-params.toml"
            Output "layers.toml"
            Output "positions.json"
            Output "supercells.json"
        Output "sp2-config.json"
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

                liftAction $ script "assemble"
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

        Output "moire.energy"
        Output "relaxed.energy"

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
            , Requires  "golden-search.yaml" (As "golden-search.yaml")
            , Records      "some.log" (From "some.log")
            , Records    "log.lammps" (From "log.lammps")
            , KeepOnError
            ] $ \tmpDir _ -> do
                gsConfig <- readYaml (tmpDir </> "golden-search.yaml")
                loudIO . eggInDir tmpDir $ doMinimization gsConfig "moire.vasp"

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

        isolate
            [ Produces        "[name].energy" (From "energy")
            -------------------------------------------------
            , Requires          "[name].vasp" (As "moire.vasp")
            , Requires          "config.json" (As "config.json")
            ] $ \tmpDir _ -> do
                energy <- loudIO . eggInDir tmpDir $ sp2Energy
                writePath (tmpDir </> "energy") (show energy ++ "\n")

        --------------------
        -- a phonopy input file with just the supercell
        "sc.conf" !> \scConf F{..} -> do
            bandConf <- needsFile "band.conf"
            readModifyWrite (filter ("DIM " `isPrefixOf`))
                            (readLines bandConf)
                            (writeLines scConf)

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

    -- -- uncomment to ENABLE uncrossing
    -- enter "uncross/[c]/[x]" $ do
    --     surrogate "run-uncross"
    --         [ ("[v]/corrected.yaml", 2)
    --         ] $ "" #> \_ F{..} -> do

    --             let density = 100 -- XXX
    --             qPath   <- needsFile "hsym.json" >>= liftIO . readQPathFromHSymJson density
    --             esNoVdw <- needsFile "novdw/eigenvalues.yaml" >>= liftIO . Phonopy.readQPathEnergies
    --             esVdw   <- needsFile   "vdw/eigenvalues.yaml" >>= liftIO . Phonopy.readQPathEnergies
    --             needSurrogateFile "init-ev-cache" "novdw/ev-cache"
    --             needSurrogateFile "init-ev-cache" "vdw/ev-cache"
    --             liftIO $
    --                 Eigenvectors.withCache (file "novdw/ev-cache") $ \(Just vsNoVdw) ->
    --                     Eigenvectors.withCache (file "vdw/ev-cache") $ \(Just vsVdw) -> do

    --                         let cfg = Uncross.UncrossConfig { Uncross.cfgQPath             = qPath
    --                                                         , Uncross.cfgOriginalEnergiesA = esNoVdw
    --                                                         , Uncross.cfgOriginalEnergiesB = esVdw
    --                                                         , Uncross.cfgOriginalVectorsA  = vsNoVdw
    --                                                         , Uncross.cfgOriginalVectorsB  = vsVdw
    --                                                         , Uncross.cfgWorkDir = file "work"
    --                                                         }
    --                         (permsNoVdw, permsVdw) <- Uncross.runUncross cfg

    --                         readModifyWrite (Phonopy.permuteBandYaml permsNoVdw)
    --                                         (readYaml (file "novdw/eigenvalues.yaml"))
    --                                         (writeYaml (file "novdw/corrected.yaml"))
    --                         readModifyWrite (Phonopy.permuteBandYaml permsVdw)
    --                                         (readYaml (file "vdw/eigenvalues.yaml"))
    --                                         (writeYaml (file "vdw/corrected.yaml"))

    -- uncomment to DISABLE uncrossing
    enter "uncross/[c]/[x]" $ do
        surrogate "run-uncross"
            [ ("[v]/corrected.yaml", 2)
            ] $ "" #> \_ F{..} -> do
                copyPath (file   "vdw/eigenvalues.yaml") (file   "vdw/corrected.yaml")
                copyPath (file "novdw/eigenvalues.yaml") (file "novdw/corrected.yaml")

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

                            let (perturbedEs, _) = Uncross.firstOrderPerturb Uncross.defaultMatchTolerances
                                                                             (unperturbedEs, unperturbedVs)
                                                                             (exactEs,       exactVs)
                            pure perturbedEs

                        readModifyWrite (Phonopy.putBandYamlSpectrum e1s)
                                        (readYaml  (file "novdw/eigenvalues.yaml"))
                                        (writeYaml (file "perturb1.yaml"))

    --------------------------------------------------
    -- HACK HACK HACK HACK HACK DUMB STUPID HACK

    informalSpec "zpol" $ do
        Input "template.yaml" -- ANY band.yaml with the right qpoints/band count
        Symlink "ev-cache"
        ----------------
        Output "out.yaml"

    enter "zpol/[c]/[x]" $ do

        -- HACK HACK HACK
        -- this is, I kid you not, a band.yaml file whose energies have been replaced with z polarization fractions
        "out.yaml" !> \_ F{..} -> do

            templateYaml <- needsFile "template.yaml"
            needSurrogateFile "init-ev-cache" "ev-cache"
            liftIO $
                Eigenvectors.withCache (file "ev-cache") $ \(Just vs) -> do

                        let sqmag y = (\x -> x*x) (Complex.magnitude y)
                        let vecZpol :: UVector (Complex.Complex Double) -> Double
                            vecZpol v = sum [ sqmag e | (i, e) <- zip [0..] (Vector.toList (Vector.convert v))
                                                      , i `mod` 3 == 2
                                                      ]
                        let zs = fmap (fmap (fmap vecZpol)) vs
                        zs' <- sequence zs

                        readModifyWrite (Phonopy.putBandYamlSpectrum ((Vector.fromList . toList) zs'))
                                        (readYaml templateYaml)
                                        (writeYaml (file "out.yaml"))

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

            cMat <- needJsonFile "coeffs.json"
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

            cMat <- needJsonFile "coeffs.json"
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

    informalSpec "unfold" $ do
        Input "data.dat"
        Input "main.gplot"
        ----------------
        Output "band.[ext]"

    "bandplot/[c]/[x]/helper" `isDirectorySymlinkTo` "input/gplot-helper"
    enter "bandplot/[c]/[x]" $ do
        "full-[ext].gplot" !> \outGPlot F{..} -> do
            -- concatenate
            lines <- List.intercalate [""] -- blank lines in-between, in case last line has "\"
                     <$> mapM (needsFile >=> readLines)
                        [ "helper/prelude.gplot"
                        , "xbase.gplot"
                        , "main.gplot"
                        , "helper/write-band-[ext].gplot"
                        ]
            writeLines outGPlot lines

        isolate
            [ Produces "band.[ext]"       (From "band.out")
            -----------------------------------------------------
            , Requires "full-[ext].gplot" (As "band.gplot")
            , Requires "data.dat"         (As "data.dat")
            ] $ \tmpDir fmt -> do
                () <- liftAction $ cmd "gnuplot band.gplot" (Cwd tmpDir)
                moveUntracked (tmpDir </> fmt "band.[ext]") (tmpDir </> "band.out") -- FIXME dumb hack


    informalSpec "improve-layer-sep" $ do
        Input "sp2-config.json"
        Input "layers.toml"
        Input "supercells.json"
        Input "spatial-params-in.toml"
        Input "linear-search.toml"
        ----------------
        Output "spatial-params-out.toml"

    -- oh dear.  This is a computation that wants to make instances of computations...
    "improve-layer-sep/[c]/[x]/[val]/sp2"      `isDirectorySymlinkTo` "sp2/comp_improve-layer-sep_[c]/[val]_[x]"
    "improve-layer-sep/[c]/[x]/[val]/assemble" `isDirectorySymlinkTo` "assemble/comp_improve-layer-sep_[c]/[val]_[x]"
    enter "improve-layer-sep/[c]/[x]" $ do

        let makeSpatialParams x = \outToml F{..} -> do
                map <- needsFile "spatial-params-in.toml" >>= readToml :: Act (Map String Double)

                let map' = Map.adjust (* x) "layer-sep" map
                -- HACK pytoml doesn't like our JSONy output, so we'll have to serve it on a silver platter
                -- writeToml outToml map'
                writeLines outToml $ (\(k,v) -> k ++ " = " ++ show v) <$> Map.toList map'

        "spatial-params-out.toml" !> \outToml F{..} -> do
            cfg <- needsFile "linear-search.toml" >>= readToml
            let objective x = do
                    fp <- needsFile (show x </> "sp2/moire.energy")
                    s <- readPath fp
                    pure (read $ idgaf s)

            best <- linearSearch objective cfg
            makeSpatialParams best outToml F{..}
            pure ()

        "[val]/assemble/spatial-params.toml" !> \fp F{..} -> makeSpatialParams (read (fmt "[val]")) fp F{..}
        "[val]/assemble/layers.toml" `isCopiedFromFile` "layers.toml"
        "[val]/sp2/config.json" `isCopiedFromFile` "sp2-config.json"
        "[val]/sp2/moire.vasp" `isLinkedFromFile` "[val]/assemble/moire.vasp"




readQPathFromHSymJson :: Int -> FileString -> IO _
readQPathFromHSymJson density fp = do
    hSymPath <- readJson fp :: IO Phonopy.HighSymInfo
    pure $ Phonopy.phonopyQPath (Phonopy.highSymInfoQPoints hSymPath)
                                (replicate (Phonopy.highSymInfoNLines hSymPath) density)

-- this awkward pattern pops up every now and then in my code and it never looked very readable to me
--  except in the most trivial cases
readModifyWrite :: (Monad m)=> (a -> b) -> m a -> (b -> m c) -> m c
readModifyWrite f read write = f <$> read >>= write

------------------------------------------------------------

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

------------------------------------------------------------

-- common pattern to perform an egg in a directory
eggInDir :: (_)=> s -> Egg a -> Egg a
eggInDir s e = singularToEgg $ pushd (idgaf s) >>= \() -> liftEgg e

script :: FileString -> PartialCmd
script x = cmd ("scripts" </> x)

------------------------------------------------------

data GoldenSearchConfig
    = NoGoldenSearch
    | DoGoldenSearch
        { goldenSearchCfgCenter :: Double
        , goldenSearchCfgUpper :: Double
        , goldenSearchCfgLower :: Double
        , goldenSearchCfgTol :: Double
        , goldenSearchCfgMemory :: Bool
        }

instance Aeson.FromJSON GoldenSearchConfig where
    parseJSON = Aeson.withObject "golden search config" $ \o ->
        o Aeson..: "enabled" >>= \case
            False -> pure $ NoGoldenSearch
            True  -> do
                goldenSearchCfgCenter <- o Aeson..: "center" -- FIXME default to 1.00
                goldenSearchCfgUpper  <- o Aeson..: "upper"
                goldenSearchCfgLower  <- o Aeson..: "lower"
                goldenSearchCfgTol    <- o Aeson..: "stop-tolerance"
                goldenSearchCfgMemory <- o Aeson..: "remember-guess"
                pure $ DoGoldenSearch{..}

instance Aeson.ToJSON GoldenSearchConfig where
    toJSON NoGoldenSearch = Aeson.object
        [ "enabled" Aeson..= False
        ]

    toJSON DoGoldenSearch{..} = Aeson.object
        [ "enabled" Aeson..= True
        , "center" Aeson..= goldenSearchCfgCenter
        , "upper"  Aeson..= goldenSearchCfgUpper
        , "lower"  Aeson..= goldenSearchCfgLower
        , "stop-tolerance"  Aeson..= goldenSearchCfgTol
        , "remember-guess"  Aeson..= goldenSearchCfgMemory
        ]
data LinearSearchConfig = LinearSearchConfig
    { linearSearchCfgLower :: Double
    , linearSearchCfgUpper :: Double
    , linearSearchCfgSteps :: [Int]
    }

instance Aeson.FromJSON LinearSearchConfig where
    parseJSON = Aeson.withObject "linear search config" $ \o -> do
        linearSearchCfgLower <- o Aeson..: "lower"
        linearSearchCfgUpper <- o Aeson..: "upper"
        linearSearchCfgSteps <- o Aeson..: "steps"
        pure $ LinearSearchConfig{..}

instance Aeson.ToJSON LinearSearchConfig where
    toJSON LinearSearchConfig{..} = Aeson.object
        [ "lower" Aeson..= linearSearchCfgLower
        , "upper" Aeson..= linearSearchCfgUpper
        , "steps" Aeson..= linearSearchCfgSteps
        ]

------------------------------------------------------

doMinimizationInit :: (MonadIO m)=> FilePath -> m ()
doMinimizationInit original = do
    cp original "POSCAR"
    setJson "config.json" ["structure_file"] $ Aeson.String "POSCAR"

doMinimization :: GoldenSearchConfig -> FilePath -> Egg ()
doMinimization NoGoldenSearch original = do
        doMinimizationInit original
        infos <- fold Fold.list $ thisMany 2 $ liftEgg sp2Relax
        echo $ "================================"
        echo $ "RELAXATION SUMMARY (NO SEARCH)"
        egg $ do
            (i, RelaxInfo energies) <- select (zip [1..] infos)
            echo $ format (" Trial "%d%": "%d%" iterations, V = "%f%" ~~> "%f)
                            i (length energies) (head energies) (last energies)
        echo $ "================================"

doMinimization DoGoldenSearch{..} original = do
        doMinimizationInit original
        ref <- liftIO (newIORef 1.0)
        () <$ goldenSearch (<)
                           (objective ref)
                           goldenSearchCfgTol
                           (goldenSearchCfgLower, goldenSearchCfgUpper)
    where
    objective :: IORef Double -> Double -> Egg Double
    objective ref scale = do
        time <- liftIO Time.getZonedTime
        echo $ "================================"
        echo $ "BEGIN AT s = " <> repr scale <> "  (" <> repr time <> ")"
        echo $ "================================"

        if goldenSearchCfgMemory
            then do
                -- reuse relaxed as guess.
                -- The IORef is used to recall the previous scale,
                -- since sp2 will have normalized the POSCAR scale to 1.
                prevScale <- liftIO $ readIORef ref
                liftIO $ writeIORef ref scale

                liftIO $ Text.readFile "POSCAR"
                        >>= pure . Poscar.fromText
                        >>= pure . (\p -> p {Poscar.scale = Poscar.Scale (scale / prevScale)})
                        >>= pure . Poscar.toText
                        >>= Text.writeFile "POSCAR"
            else do
                -- use original structure as guess
                liftIO $ Text.readFile (idgaf original)
                        >>= pure . Poscar.fromText
                        >>= pure . (\p -> p {Poscar.scale = Poscar.Scale scale})
                        >>= pure . Poscar.toText
                        >>= Text.writeFile "POSCAR"

        infos <- fold Fold.list $ thisMany 2 $ liftEgg sp2Relax

        echo $ "================================"
        echo $ "RELAXATION SUMMARY AT s = " <> repr scale
        egg $ do
            (i, RelaxInfo energies) <- select (zip [1..] infos)
            echo $ format (" Trial "%d%": "%d%" iterations, V = "%f%" ~~> "%f)
                            i (length energies) (head energies) (last energies)
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
sp2Relax = (setPhonopyState False False False False False Nothing >>) . liftEgg $ do
    (out,_) <- addStrictOut sp2
    pure $ parseRelaxInfoFromSp2 out

sp2Energy :: Egg Double
sp2Energy = (setPhonopyState False False False False False (Just 1) >>) . liftEgg $ do
    (out,_) <- addStrictOut sp2
    pure . head . relaxInfoStepEnergies $ parseRelaxInfoFromSp2 out

sp2Displacements :: Egg ()
sp2Displacements = setPhonopyState True False False False False (Just 1) >> sp2

sp2Forces :: Egg ()
sp2Forces = setPhonopyState False True True False False (Just 1) >> sp2

sp2Raman :: Egg ()
sp2Raman = setPhonopyState False False False True False (Just 1) >> sp2

setPhonopyState :: (_)=> Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Int -> io ()
setPhonopyState d f b r i c = liftIO $ do
    setJson "config.json" ["phonopy", "calc_displacements"] $ Aeson.Bool d
    setJson "config.json" ["phonopy", "calc_force_sets"] $ Aeson.Bool f
    setJson "config.json" ["phonopy", "calc_bands"] $ Aeson.Bool b
    setJson "config.json" ["phonopy", "calc_raman"] $ Aeson.Bool r
    setJson "config.json" ["phonopy", "calc_irreps"] $ Aeson.Bool i
    whenJust c $ \c' ->
        setJson "config.json" ["phonopy", "minimize", "iteration_limit"] $ Aeson.Number (fromIntegral c')

parseRelaxInfoFromSp2 :: Text -> RelaxInfo
parseRelaxInfoFromSp2 = RelaxInfo . mapMaybe iterationEnergy . Text.lines
  where
    mapMaybe f = map f >>> filter isJust >>> sequence >>> maybe undefined id
    -- sp2's output is pretty varied so we use a very specific (and therefore fragile) filter
    iterationEnergy s | "    Value:" `Text.isPrefixOf` s = s & Text.words & (!! 1) & Text.Read.double & either error fst & Just
                      |        " i:" `Text.isPrefixOf` s = s & Text.words & (!! 3) & Text.Read.double & either error fst & Just
                      | otherwise = Nothing


-- linear search for minimum
linearSearch :: (Monad m)
             => (Double -> m Double)
             -> LinearSearchConfig
             -> m Double
linearSearch objective LinearSearchConfig{..} =
    case linearSearchCfgSteps of
        [] -> pure $ 0.5 * (linearSearchCfgLower + linearSearchCfgUpper)
        (curSteps:restSteps) -> do
            -- Make N + 2 points and find the best among the inner N
            let fracs = [ fromIntegral i / fromIntegral (curSteps-1) | i <- [-1..curSteps] ]
            let xs = [ (1 - x) * linearSearchCfgLower + x * linearSearchCfgUpper | x <- fracs ]
            values <- mapM objective xs
            let bestIdx = snd $ minimum $ tail . init $ zip values [0..]
            linearSearch objective $ LinearSearchConfig
                { linearSearchCfgLower = xs !! pred bestIdx
                , linearSearchCfgUpper = xs !! succ bestIdx
                , linearSearchCfgSteps = restSteps
                }
