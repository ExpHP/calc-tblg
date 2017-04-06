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
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
--     Use Eggshell/Egg (the Turtle-based layer) for any new impure
--      transformations.

--  *  Only types derived from Shake.Action (such as Act) retain dependencies.
--     I.e. anything that runs in an eggshell or egg has no automatically
--          tracked dependencies.

import           Prelude hiding (FilePath, interact)
import           "base" Control.Applicative
import           "base" Data.IORef
import qualified "base" Data.List as List
import           "base" System.IO.Error(isDoesNotExistError)
import           "base" Control.Arrow((>>>))
import           "mtl" Control.Monad.Identity
import qualified "shake" Development.Shake as Shake
import           "shake" Development.Shake.FilePath(normaliseEx)
import qualified "filepath" System.FilePath.Posix as Shake((</>))
import           "directory" System.Directory(createDirectoryIfMissing)
import           "exceptions" Control.Monad.Catch(handleIf)
import           "containers" Data.Set(Set)
import qualified "containers" Data.Set as Set
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.Encoding as Text
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
import           ShakeUtil hiding ((%>))
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
        let Identity func = (iDontCare . Subst.compile) "[p]:::[v]:::[k]"
                            >>= (iDontCare . flip Subst.substIntoFunc pat)

        let pvks = List.intercalate ":::" <$> sequence [ps,vs,ks]
        need $ unique (mapMaybe func pvks)

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
            eggIO $ setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj

        "vdw/config-true.json"   !> configRule True
        "novdw/config-true.json" !> configRule False
        "[v]/moire-true.[ext]"   `isCopiedFromFile` "input/moire.[ext]"

        enter "[v]" $ do
            surrogate "optimization"
                [ ("relaxed.vasp", 1)
                ] $ "" #> \root F{..} -> do

                    copyPath (file "config-true.json") (file "config.json")
                    copyPath (file "moire-true.vasp")  (file "moire.vasp")
                    copyPath (file "moire-true.xyz")   (file "moire.xyz")
                    eggIO . eggInDir root $ doMinimization "moire.vasp"
                    copyUntracked (file "moire.vasp") (file "relaxed.vasp")

            surrogate "displacements"
                [ ("POSCAR-[x]", 1)
                , ("band.conf", 1)
                ] $ "" #> \root F{..} -> do
                    needSurrogate "optimization" root
                    copyPath (file "relaxed.vasp") (file "moire.vasp")
                    eggIO . eggInDir root $ sp2Displacements

            surrogate "force-constants"
                [ ("FORCE_CONSTANTS", 1)
                , ("eigenvalues.yaml", 1)
                , ("band_labels.txt", 1)
                ] $ "" #> \root F{..} -> do
                    needSurrogate "displacements" root
                    copyPath (file "relaxed.vasp") (file "moire.vasp")
                    eggIO . eggInDir root $ sp2Forces
                    copyUntracked (file "band.yaml") (file "eigenvalues.yaml")

            surrogate "raman"
                [ ("gauss_spectra.dat", 1)
                ] $ "" #> \root F{..} -> do
                    needSurrogate "force-constants" root
                    copyPath (file "relaxed.vasp") (file "moire.vasp")
                    eggIO . eggInDir root $ sp2Raman

    -- gnuplot data is preparsed into lightning-fast JSON.
    -- json files will be ordered by column (x/y), then hsym line, then kpoint
    let degnuplot :: PartialCmd
        degnuplot = script "degnuplot -Gbhkx -Jxhkb"
    let engnuplot :: PartialCmd
        engnuplot = script "engnuplot -Gbhkx -Jxhkb"

    enter "[p]" $ do
        enter "[v]" $ do

            -------------------
            -- data.dat

            "data-orig.dat" !> \dataDat F{..} -> do
                needFile ["FORCE_CONSTANTS"]
                liftAction $ cmd "bandplot --gnuplot" (Cwd $ file "") (FileStdout dataDat)

            -- Parse gnuplot into JSON with high-symmetry point first
            "data-orig.json" !> \json F{..} -> do
                dat <- needsFile "data-orig.dat"
                liftAction $ degnuplot (FileStdin dat) (FileStdout json)

            -- Reorder between highsym points to remove crossings.
            "data-untangle.json" %> \F{..} -> do
                [x, y] <- needJSON (file "data-orig.json") :: Act [[[[Double]]]]
                pure $ [x, fst $ reorder y]

            -- Convert back to gnuplot
            "data.json" `isCopiedFromFile` "data-untangle.json"
            "data.dat" !> \dat F{..} -> do
                json <- needsFile "data.json"
                liftAction $ engnuplot (FileStdin json) (FileStdout dat)

    enter "[p]" $ do
        enter "[v]" $ do

            --------------------
            -- a phonopy input file with just the supercell
            "sc.conf" !> \scConf F{..} -> do
                bandConf <- needsFile "band.conf"
                (head <$> readLines bandConf) >>= writePath scConf

            "band-[k].yaml" !> \_ F{..} -> do
                needFile ["sc.conf"]
                needFile ["FORCE_CONSTANTS"]

                eggIO . eggInDir (file "") . egg $ do

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

            "freqs/[k]" !> \freqOut F{..} -> do
                let Just i = List.elemIndex (fmt "[k]") kpointShorts -- HACK should use order in data

                [(_,y)] <- needDataDat [file "data.json"]
                writeJSON freqOut $ fmap ((! i) >>> (! 0)) y


    enter "[p]" $ do

        ".post/bandplot/data-vdw.[ext]"   `isHardLinkToFile` "vdw/data.[ext]"
        ".post/bandplot/data-novdw.[ext]" `isHardLinkToFile` "novdw/data.[ext]"
        enter ".post/bandplot" $ do
            "data-both.dat" !> \dataBoth F{..} -> do
                [(xs, ysN)] <- needDataDat [file "data-novdw.json"]
                [(_,  ysV)] <- needDataDat [file "data-vdw.json"]
                let out = idgaf $ Aeson.encode [xs, ysN, ysV]
                liftAction $ engnuplot (Stdin out) (FileStdout dataBoth)

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
                head <$> readLines (file "input/moire.vasp")
                  >>= writePath title
        ".post/bandplot/data-prelude.dat" !> \prelude F{..} ->
                (take 3 <$> readLines (file "vdw/data-orig.dat"))
                >>= writeLines prelude

        ".post/bandplot/band_labels.txt"      `isCopiedFromFile` "vdw/band_labels.txt"
        ".post/bandplot/vdw.gplot.template"   `isCopiedFromFile` "input/band.gplot.template"
        ".post/bandplot/novdw.gplot.template" `isCopiedFromFile` "input/band.gplot.template"
        ".post/bandplot/both.gplot.template"  `isCopiedFromFile` "input/both.gplot.template"
        enter ".post/bandplot" $ do
            "band_xticks.txt" !> \xvalsTxt F{..} -> do
                -- third line has x positions.  First character is '#'.
                dataLines <- readLines (file "data-prelude.dat")
                let counts = words . tail $ idgaf (dataLines !! 2)
                labels <- words <$> readPath (file "band_labels.txt")

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
                template <- fmap idgaf <$> readLines (file "[s].gplot.template")
                writeLines bandGplot $ topLines <> template

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

    enter "[p]" $ do
        enter "[v]" $ do
            "band-[k].json" !> \json F{..} -> do
                yaml <- needsFile "band-[k].yaml"
                liftAction $ script "eigenvectors-alt" yaml (FileStdout json)

        "out/wobble/[k]-[b]-[v].xyz" !> \outXyz F{..} -> do
            bandYaml <- needsFile "[v]/band-[k].yaml"
            Just supercell <- getJson (file "supercells.json") ["wobble"]
            supercell <- pure $ (supercell ^.. Aeson.values . Aeson._Integral :: [Int])

            liftAction $ cmd "wobble" bandYaml
                    "--supercell" (show supercell)
                    "--bands"     (fmt "[b]")
                    (FileStdout outXyz)

    liftIO $ createDirectoryIfMissing True "out/bands"
    "out/bands/[p]-[s].[ext]" `isCopiedFromFile` "[p]/.post/bandplot/[s].[ext]"

data SerdeFuncs a = SerdeFuncs
  { sfRule :: Pat -> (Fmts -> Act a) -> App ()
  , sfNeed :: [FileString] -> Act [a]
  }

-- Make a pair of `!>` and `need` functions that work with a JSON serialized
-- datatype.
serdeFuncs :: (Aeson.ToJSON a, Aeson.FromJSON a)=> SerdeFuncs a
serdeFuncs = SerdeFuncs sfRule sfNeed
  where
    sfRule = (%>)
    sfNeed paths = need paths >> forM paths readJSON

writeJSON :: (Aeson.ToJSON a, MonadIO io)=> FileString -> a -> io ()
writeJSON dest value = liftIO $ ByteString.Lazy.writeFile dest (Aeson.encode value)
readJSON :: (Aeson.FromJSON a, MonadIO m)=> FileString -> m a
readJSON s = do
    value <- Aeson.eitherDecode <$> liftIO (ByteString.Lazy.readFile s)
    either fail pure value

needJSON :: (_)=> FileString -> Act a
needJSON s = head <$> sfNeed serdeFuncs [s]

(%>) :: _ => Pat -> (Fmts -> Act a) -> App ()
pat %> act = pat !> \dest fmts ->
    act fmts >>= writeJSON dest

moveUntracked :: (MonadIO io)=> FilePath -> FilePath -> io ()
moveUntracked = mv
copyUntracked :: (MonadIO io)=> FilePath -> FilePath -> io ()
copyUntracked = cp

allPatterns :: Act [FileString]
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

script :: FileString -> PartialCmd
script x = cmd ("scripts" Shake.</> x)
------------------------------------------------------

doMinimization :: FilePath -> Egg ()
doMinimization original = do
                             init
                             ref <- liftIO (newIORef 1.0)
                             goldenSearch (<) (objective ref)
                                         -- enable minimization:
                                         --(1e-3) (0.97,1.03)
                                         -- disable minimization:
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

iDontCare :: (Monad m)=> Either String a -> m a
iDontCare = either fail pure -- https://www.youtube.com/watch?v=ZXsQAXx_ao0
