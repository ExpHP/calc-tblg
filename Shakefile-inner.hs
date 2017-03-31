-- Shakefile
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Prelude hiding (FilePath, interact)
import           "base" Control.Applicative
import           "base" Data.IORef
import qualified "base" Data.List as List
import           "base" System.IO.Error(isDoesNotExistError)
import           "base" Control.Arrow((>>>))
import           "mtl" Control.Monad.Identity
import           "shake" Development.Shake hiding (need, needed, (%>))
import           "shake" Development.Shake.Command
import           "shake" Development.Shake.FilePath(normaliseEx)
import qualified "filepath" System.FilePath.Posix as Shake((</>))
import           "directory" System.Directory(createDirectoryIfMissing)
import           "exceptions" Control.Monad.Catch(handleIf)
import           "containers" Data.Set(Set)
import qualified "containers" Data.Set as Set
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.IO as Text.IO
import qualified "text" Data.Text.Read as Text.Read
import qualified "bytestring" Data.ByteString.Lazy as ByteString.Lazy
import           "lens" Control.Lens hiding ((<.>), strict)
import qualified "foldl" Control.Foldl as Fold
import           "vector" Data.Vector(Vector, (!))
import qualified "aeson" Data.Aeson as Aeson
import qualified "lens-aeson" Data.Aeson.Lens as Aeson
import qualified "vasp-poscar" Data.Vasp.Poscar as Poscar
import           "turtle-eggshell" Eggshell hiding (need,view)
import qualified "terrible-filepath-subst" Text.FilePath.Subst as Subst
import qualified Turtle.Please as Turtle
import           JsonUtil
import           ShakeUtil
import           BandAssocs

opts :: ShakeOptions
opts = shakeOptions
    { shakeFiles     = ".shake/"
    , shakeVerbosity = Diagnostic
    }

-- NOTE: these data types are used for automatic serialization.
--       layout matters!
-- data for gnuplot, indexed by:   band, hsymline, kpoint
type BandGplData a = (Vector (Vector (Vector a)))
type BandGplColumn = BandGplData Double

main :: IO ()
main = shakeArgs opts $ do

    let SerdeFuncs { sfRule=dataDatRule
                   , sfNeed=needDataDat
                   } = serdeFuncs :: SerdeFuncs (BandGplColumn, BandGplColumn)

    let SerdeFuncs { sfRule=bandAssocsRule
                   , sfNeed=needBandAssocs
                   } = serdeFuncs :: SerdeFuncs (BandAssocs Int Int)

    -- let the user supply their own pattern.
    -- [p], [v], and [k] will iterate over the values they are
    --   usually expected to have in the patterns used in this file.
    "all:[:**]" ~!> \('a':'l':'l':':':pat) _ -> do
        ps <- allPatterns
        vs <- pure ["vdw", "novdw"]
        ks <- pure kpointShorts

        let unique = Set.toList . Set.fromList
        let mapMaybe f xs = f <$> xs >>= maybe [] pure
        let Identity func = Subst.compile "[p]:::[v]:::[k]"
                            >>= (`Subst.substIntoFunc` pat)

        let pvks = List.intercalate ":::" <$> sequence [ps,vs,ks]
        need $ unique (mapMaybe func pvks)

    let makeStructure :: [String] -> _
        makeStructure extra path (Fmt fmt) = do
        posJson    <- needs $ fmt "[p]/positions.json"
        paramsToml <- needs $ fmt "[p]/spatial-params.toml"

        cmd "../../scripts/make-poscar" extra
                "-S" paramsToml "-P" posJson
                (FileStdout path)

    "graphene/input/moire.vasp" !> makeStructure ["--graphene"]
    "graphene/input/moire.xyz"  !> makeStructure ["--graphene", "--xyz"]
    "[p]/input/moire.vasp" !> makeStructure []
    "[p]/input/moire.xyz"  !> makeStructure ["--xyz"]

    let configRule lj = \path (Fmt fmt) -> do
        copyFile' (fmt "[]/input/config.json") path
        eggIO $ setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj

    -- HACK:  file.ext vs file-true.ext:
    --     - file-true.ext  is the one tracked in Shake's dependency graph
    --     - file.ext       is the actual input to computations, and gets modified
    --
    -- A better solution would be temp dirs for computation.
    "[]/vdw/config-true.json"   !> configRule True
    "[]/novdw/config-true.json" !> configRule False
    "[]/vdw/moire-true.[]"   `isCopiedFrom` "[]/input/moire.[]"
    "[]/novdw/moire-true.[]" `isCopiedFrom` "[]/input/moire.[]"

    -- rules involving phonopy use surrogates to hide the fact that they
    -- perform modifications to shared files.
    -- these surrogates must be called in a particular sequence, which is
    -- ensured by their dependencies.
    -- Temporary directories would be a nicer solution.
    surrogate "optimization"
        [ ("relaxed.vasp", 1)
        ] $ "[]/[]" #> \root (Fmt fmt) -> do

            need $ fmap fmt
                [ "[]/[]/moire-true.vasp"
                , "[]/[]/moire-true.xyz"
                , "[]/[]/config-true.json"
                ]
            eggIO . eggInDir root $ do
                cp "config-true.json" "config.json"
                cp "moire-true.vasp"  "moire.vasp"
                cp "moire-true.xyz"   "moire.xyz"
                doMinimization "moire.vasp"
                cp "moire.vasp" "relaxed.vasp"

    surrogate "displacements"
        [ ("POSCAR-[]", 1)
        , ("band.conf", 1)
        ] $ "[]/[]" #> \root (Fmt fmt) -> do
            needSurrogate "optimization" root
            copyFile' (fmt "[]/[]/relaxed.vasp") (fmt "[]/[]/moire.vasp")
            eggIO . eggInDir root $ sp2Displacements

    surrogate "force-constants"
        [ ("FORCE_CONSTANTS", 1)
        , ("eigenvalues.yaml", 1)
        ] $ "[]/[]" #> \root (Fmt fmt) -> do
            needSurrogate "displacements" root
            copyFile' (fmt "[]/[]/relaxed.vasp") (fmt "[]/[]/moire.vasp")
            eggIO . eggInDir root $ do
                sp2Forces
                cp "band.yaml" "eigenvalues.yaml"

    surrogate "raman"
        [ ("gauss_spectra.dat", 1)
        ] $ "[]/[]" #> \root (Fmt fmt) -> do
            needSurrogate "force-constants" root
            copyFile' (fmt "[]/[]/relaxed.vasp") (fmt "[]/[]/moire.vasp")
            eggIO . eggInDir root $ sp2Raman

    -------------------
    -- data.dat

    "[]/[]/data-orig.dat" !> \dataDat (Fmt fmt) -> do
        need [fmt "[]/[]/FORCE_CONSTANTS"]
        cmd "bandplot --gnuplot" (Cwd $ fmt "[]/[]") (FileStdout dataDat)

    -- Parse gnuplot into JSON with high-symmetry point first
    "[]/[]/data-orig.json" !> \json (Fmt fmt) -> do
        dat <- needs (fmt "[]/[]/data-orig.dat")
        script "degnuplot" "-Gbhkx" "-Jxhbk" (FileStdin dat) (FileStdout json)

    -- Reorder between highsym points to remove crossings.
    "[]/[]/data-untangle.json" %> \(Fmt fmt) -> do
        [x, y] <- needJSON (fmt "[]/[]/data-orig.json") :: Action [[[[Double]]]]
        pure $ [x, fst $ reorder y]

    -- Convert back to gnuplot
    "[p]/[v]/data.json" `isCopiedFrom` "[p]/[v]/data-untangle.json"
    "[p]/[v]/data.dat" !> \dat (Fmt fmt) -> do
        json <- needs (fmt "[p]/[v]/data.json")
        script "engnuplot" "-Gbhkx" "-Jxhbk" (FileStdin json) (FileStdout dat)

    --------------------
    -- a phonopy input file with just the supercell
    "[]/[]/sc.conf" !> \scConf (Fmt fmt) -> do
        bandConf <- needs (fmt "[]/[]/band.conf")
        (head <$> readFileLines bandConf) >>= writeFile' scConf

    "[]/[]/band-[k].yaml" !> \_ (Fmt fmt) -> do
        need [fmt "[]/[]/sc.conf"]
        need [fmt "FORCE_CONSTANTS"]
        eggIO . eggInDir (fmt "[]/[]") . egg $ do

            let kshort = fmt "[k]"
            let kloc = kpointLoc kshort :: [Double]
            let kstr = Text.intercalate " " $ map repr $ kloc
            procs "phonopy"
                [ "sc.conf"
                , "--readfc"
                , "--eigenvectors"
                , "--band_points=1"
                , "--band=" <> kstr <> " " <> kstr
                ] empty
            mv "band.yaml" (fmt "band-[k].yaml")

    "[p]/[v]/freqs/[k]" !> \freqOut (Fmt fmt) -> do
        let Just i = List.elemIndex (fmt "[k]") kpointShorts -- HACK should use order in data

        [(_,y)] <- needDataDat [fmt "[p]/[v]/data.json"]
        writeJSON freqOut $ fmap ((! i) >>> (! 0)) y

    "[p]/[v]/band_xticks.txt" !> \xvalsTxt (Fmt fmt) -> do
        -- third line has x positions.  First character is '#'.
        dataLines <- readFileLines (fmt "[p]/[v]/data.dat")
        let counts = words . tail $ idgaf (dataLines !! 2)
        labels <- words <$> readFile' (fmt "[p]/[v]/band_labels.txt")

        let dquote = \s -> "\"" ++ s ++ "\""
        let paren  = \s -> "("  ++ s ++ ")"
        writeFile' xvalsTxt
            ("set xtics " ++ paren
                (List.intercalate ", "
                    (List.zipWith (\l a -> dquote l ++ " " ++ a)
                        labels counts)))

    let gplotXBase :: _ -> _ -> _ -> Action [String]
        gplotXBase dataFile titleFile ticksFile = do
            xticksLine <- readFile' ticksFile
            title <- readFile' titleFile

            let dquote = \s -> "\"" ++ s ++ "\""
            pure
                [ "set title " ++ dquote (idgaf title)
                , xticksLine
                , "band_n = 3" -- FIXME
                , "data = " ++ dquote ((idgaf.filename.idgaf) dataFile)
                ]

    "[p]/[v]/title" !> \title (Fmt fmt) ->
        head <$> readFileLines (fmt "[p]/input/moire.vasp")
            >>= writeFile' title

    "[p]/[v]/band.gplot" !> \bandGplot (Fmt fmt) -> do
        templateFile <- needs (fmt "[p]/input/band.gplot.template")

        topLines <- gplotXBase <$> needs (fmt "[p]/[v]/data.dat")
                               <*> needs (fmt "[p]/[v]/title"   )
                               <*> needs (fmt "[p]/[v]/band_xticks.txt")
                               & join
        template <- fmap idgaf <$> readFileLines templateFile
        writeFileLines bandGplot $ topLines <> template

    "[p]/.post/data-[v].dat" `isCopiedFrom` "[p]/[v]/data.dat"
    "[p]/.post/data-both.dat" !> \dataBoth (Fmt fmt) -> do
        [(xs, ysN)] <- needDataDat [fmt "[p]/.post/data-novdw.json"]
        [(_,  ysV)] <- needDataDat [fmt "[p]/.post/data-vdw.json"]
        let out = idgaf $ Aeson.encode [xs, ysN, ysV]
        script "engnuplot" "-z" (Stdin out) (FileStdout dataBoth)

    "[p]/.post/both.gplot" !> \bandGplot (Fmt fmt) -> do
        templateFile <- needs (fmt "[p]/input/both.gplot.template")

        topLines <- gplotXBase <$> needs (fmt "[p]/.post/data-both.dat")
                               <*> needs (fmt "[p]/vdw/title"   )
                               <*> needs (fmt "[p]/vdw/band_xticks.txt")
                               & join
        template <- fmap idgaf <$> readFileLines templateFile
        writeFileLines bandGplot $ topLines <> template

    family
        [ "[p]/[v]/band.png"
        , "[p]/[v]/band.svg"
        ] &!> \_ (Fmt fmt) -> do
            gplot <- needs $ fmt "[p]/[v]/band.gplot"
            cmd "gnuplot" (Cwd $ fmt "[p]/[v]") (FileStdin gplot)

    family
        [ "[p]/.post/both.png"
        , "[p]/.post/both.svg"
        ] &!> \_ (Fmt fmt) -> do
            gplot <- needs $ fmt "[p]/.post/both.gplot"
            cmd "gnuplot" (Cwd $ fmt "[p]/.post") (FileStdin gplot)

    "[p]/out/wobble/[k]-[b]-[v].xyz" !> \outXyz (Fmt fmt) -> do
        bandYaml <- needs $ fmt "[p]/[v]/band-[k].yaml"
        Just supercell <- getJson "[p]/supercells.json" ["wobble"]
        supercell <- pure $ (supercell ^.. Aeson.values . Aeson._Integral :: [Int])

        cmd "wobble" bandYaml
                "--supercell" (show supercell)
                "--bands"     (fmt "[b]")
                (FileStdout outXyz)

    liftIO $ createDirectoryIfMissing True "out/bands"
    "out/bands/[p]-vdw.[ext]"   `isCopiedFrom` "[p]/vdw/band.[ext]"
    "out/bands/[p]-novdw.[ext]" `isCopiedFrom` "[p]/novdw/band.[ext]"
    "out/bands/[p]-both.[ext]"  `isCopiedFrom` "[p]/.post/both.[ext]"

    "[p]/[v]/band-[k].json" !> \json (Fmt fmt) -> do
        yaml <- needs (fmt "[p]/[v]/band-[k].yaml")
        script "eigenvectors-alt" yaml (FileStdout json)

data SerdeFuncs a = SerdeFuncs
  { sfRule :: Pat -> (Fmt -> Action a) -> Rules ()
  , sfNeed :: [FileString] -> Action [a]
  }

-- Make a pair of `!>` and `need` functions that work with a JSON serialized
-- datatype.
serdeFuncs :: (Aeson.ToJSON a, Aeson.FromJSON a)=> SerdeFuncs a
serdeFuncs = SerdeFuncs sfRule sfNeed
  where
    sfRule = (%>)
    sfNeed paths = do
        need paths
        forM paths $ \s -> do
            value <- Aeson.eitherDecode <$> liftIO (ByteString.Lazy.readFile s)
            either fail pure value

writeJSON :: (Aeson.ToJSON a)=> FileString -> a -> Action ()
writeJSON dest value = liftIO $ ByteString.Lazy.writeFile dest (Aeson.encode value)
needJSON :: (_)=> FileString -> Action a
needJSON s = head <$> sfNeed serdeFuncs [s]

(%>) :: _ => Pat -> (Fmt -> Action a) -> Rules ()
pat %> act = pat !> \dest fmt ->
    act fmt >>= writeJSON dest


allPatterns :: Action [FileString]
allPatterns =
    eggIO . fold Fold.list $ (reverse . List.dropWhile (== '/') . reverse) .
        normaliseEx . idgaf . parent <$> glob "*/positions.json"

-- FIXME I wanted to read these from JSON now that we're out of the bash tarpit.
--       (In fact, dynamically-generated dependencies is Shake's specialty!)
kpointShorts :: [[Char]]
kpointShorts = ["g", "m", "k"]
kpointLoc :: (Fractional t, IsString a, Eq a) => a -> [t]
kpointLoc "g" = [0, 0, 0]
kpointLoc "m" = [1/2, 0, 0]
kpointLoc "k" = [1/3, 1/3, 0]
kpointLoc _   = error "bugger off ghc"

--    "//irreps-*.yaml" !> \dest [stem, kpoint] -> runPhonopyIrreps stem kpoint dest

-- common pattern to perform an egg in a directory
eggInDir :: (_)=> s -> Egg a -> Egg a
eggInDir s e = singularToEgg $ pushd (idgaf s) >>= \() -> liftEgg e

script :: (CmdArguments args)=> FileString -> args :-> Action r
script x = cmd ("scripts" Shake.</> x)

------------------------------------------------------

doMinimization :: FilePath -> Egg ()
doMinimization original = do
                             init
                             ref <- liftIO (newIORef 1.0)
                             goldenSearch (<) (objective ref)
                                         --(1e-3) (0.97,1.03)
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

--------------------------------
-- UTILS: JSON FILE MANIPULATION
--------------------------------
---------------------
-- UTILS: MORE SHELLS
---------------------

-- | @cp -a src/* dest@.  "Dumb" because it will fail if the trees have any
--   similar substructure (i.e. src\/subdir and dest\/subdir)
mergetreeDumb :: (_)=> FilePath -> FilePath -> egg ()
mergetreeDumb src dest = egg $ do
    entry <- ls src
    cptree entry (dest <> filename entry)

-- | @cp -f --backup=numbered@
cpBackup :: (_)=> FilePath -> FilePath -> egg ()
cpBackup src dest = procs "cp"
    [ "-f", "--backup=numbered"
    , idgaf src, idgaf dest
    ] empty

-- FIXME Shell doesn't implement MonadCatch, I wonder why?
--       The code gets around this by annotating 'rmtree path :: IO ()' and adding an
--       explicit 'liftIO' at the end, but what are the implications?
-- | @rm -rf dir@
cleartree :: (MonadIO io)=> FilePath -> io ()
cleartree path = liftIO $ handleIf isDoesNotExistError ignoreIt (rmtree path :: IO ())
    where ignoreIt = const (pure ())

-- | @rm -rf dir && mkdir dir@
renewtree :: (MonadIO io)=> FilePath -> io ()
renewtree path = cleartree path >> mkdir path

-- | @cp -a src dest@
cptree :: (_)=> FilePath -> FilePath -> egg ()
cptree src dest = procs "cp" ["-a", idgaf src, idgaf dest] empty

tprocs :: (_)=> Text -> [Text] -> Shell Line -> egg ()
tprocs cmd args input = do
    err (Text.intercalate " " (cmd:args))
    procs cmd args input
