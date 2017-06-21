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
import           "base" Control.Monad.Zip
import qualified "base" Data.List as List
import           "filepath" System.FilePath.Posix((</>))
import           "directory" System.Directory(createDirectoryIfMissing)
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.IO as Text
import qualified "vector" Data.Vector as Vector
import qualified "aeson" Data.Aeson as Aeson
import qualified "aeson" Data.Aeson.Types as Aeson
import           "turtle-eggshell" Eggshell hiding (need,view,empty,(</>))

-- import qualified Turtle.Please as Turtle hiding (empty)
import           Rules.Comp(componentRules)
import           Rules.Meta(wrappedMain)
import           JsonUtil
import           GeneralUtil(onlyUniqueValue)
import           FunctorUtil(wrapZip, unwrapZip, ffmap, fffmap, ffffmap)
import           ShakeUtil hiding (doesFileExist)

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
main = wrappedMain shakeCfg appCfg allRules

allRules :: App ()
allRules = do
    mainRules
    componentRules
    plottingRules

-------------------------------------------

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
    "work/[p]/bandplot"     `isDirectorySymlinkTo` "bandplot/pat/[p]"
    "work/[p]/zpol.[v]"     `isDirectorySymlinkTo` "zpol/pat/[p].[v]"
    "work/[p]/xypol.[v]"    `isDirectorySymlinkTo` "xypol/pat/[p].[v]"
    "uncross/pat/[p]/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/pat/[p].[v]/"
    "perturb1/pat/[p]/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/pat/[p].[v]/"
    "zpol/pat/[p].[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/pat/[p].[v]/"

    "work/[p]/hsym.json"   `isCopiedFromFile` "input/hsym.json"
    "work/[p]/config.json" `isHardLinkToFile` "input/sp2-config.json"

    ------------------------------
    -- Now, we can hook up all of the input and output files.
    -- We can freely use our symlinks; there are mechanisms built into our 'App' monad
    --  which ensure that the dependency tree we present to Shake has the correct structure.

    enter "work/[p]" $ do

        -- FIXME exorcise the ab subdirs
        "positions.json"               `isHardLinkToFile` "pat/ab/positions.json"
        "supercells.json"              `isHardLinkToFile` "pat/ab/supercells.json"
        "assemble/spatial-params.toml" `isCopiedFromFile` "pat/ab/spatial-params.toml"
        "assemble/layers.toml"         `isCopiedFromFile` "pat/ab/layers.toml"

        let configRule lj = \path F{..} -> do
            Just supercell <- needsFile "supercells.json" >>= flip getJson ["phonopy"] . idgaf
            copyPath (file "config.json") path
            setJson (idgaf path) ["phonopy","supercell_dim"] supercell
            setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj
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
        "bandplot/input-data-[v]-orig.dat"        `datIsConvertedFromYaml` "uncross/[v]/eigenvalues.yaml"
        "bandplot/input-data-[v]-zpol.dat"        `datIsConvertedFromYaml` "zpol.[v]/out.yaml"
        "bandplot/input-data-[v]-folded-ab.dat"   `datIsConvertedFromYaml` "fold.[v]/out.yaml"
        "bandplot/input-data-[v]-unfolded-ab.dat" `datIsConvertedFromYaml` "unfold.[v]/out.yaml"
        "bandplot/input-data-[v]-perfect-ab.dat"  `datIsConvertedFromYaml` "perfect-ab-sp2.[v]/eigenvalues.yaml"
        "bandplot/input-data-perturb1.dat"        `datIsConvertedFromYaml` "perturb1/perturb1.yaml"
    -- FIXME doesn't seem to register for mysterious reasons
    -- "zpol.[v]/template.yaml"              `isCopiedFromFile` "sp2.[v]/eigenvalues.yaml"
    "zpol/pat/[p].[v]/template.yaml"              `isCopiedFromFile` "sp2/pat/[p].[v]/eigenvalues.yaml"

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

    ---------------------------------------------

    "abc/[p]/input"          `isDirectorySymlinkTo` "input/abc-rot/[p]"
    "aba/[p]/input"          `isDirectorySymlinkTo` "input/aba-rot/[p]"

    "abc/[p]/assemble"     `isDirectorySymlinkTo` "assemble/abc/[p]"
    "abc/[p]/ev-cache.[v]" `isDirectorySymlinkTo` "ev-cache/abc/[p].[v]"
    "abc/[p]/sp2.[v]"      `isDirectorySymlinkTo` "sp2/abc/[p].[v]"
    "abc/[p]/uncross"      `isDirectorySymlinkTo` "uncross/abc/[p]"
    "abc/[p]/perturb1"     `isDirectorySymlinkTo` "perturb1/abc/[p]"
    "abc/[p]/bandplot"     `isDirectorySymlinkTo` "bandplot/abc/[p]"
    "uncross/abc/[p]/[v]/ev-cache"  `isDirectorySymlinkTo` "ev-cache/abc/[p].[v]/"
    "perturb1/abc/[p]/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/abc/[p].[v]/"

    "aba/[p]/assemble"     `isDirectorySymlinkTo` "assemble/aba/[p]"
    "aba/[p]/ev-cache.[v]" `isDirectorySymlinkTo` "ev-cache/aba/[p].[v]"
    "aba/[p]/sp2.[v]"      `isDirectorySymlinkTo` "sp2/aba/[p].[v]"
    "aba/[p]/uncross"      `isDirectorySymlinkTo` "uncross/aba/[p]"
    "aba/[p]/perturb1"     `isDirectorySymlinkTo` "perturb1/aba/[p]"
    "aba/[p]/bandplot"     `isDirectorySymlinkTo` "bandplot/aba/[p]"
    "uncross/aba/[p]/[v]/ev-cache"  `isDirectorySymlinkTo` "ev-cache/aba/[p].[v]/"
    "perturb1/aba/[p]/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/aba/[p].[v]/"

    "aba/[p]/zpol.[v]"     `isDirectorySymlinkTo` "zpol/aba/[p].[v]"
    "abc/[p]/zpol.[v]"     `isDirectorySymlinkTo` "zpol/abc/[p].[v]"
    "zpol/aba/[p].[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/aba/[p].[v]"
    "zpol/abc/[p].[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/abc/[p].[v]"

    "aba/[p]/hsym.json"   `isCopiedFromFile` "input/hsym.json"
    "abc/[p]/hsym.json"   `isCopiedFromFile` "input/hsym.json"

    "aba/[p]/config.json" `isHardLinkToFile` "input/sp2-config.json"
    "abc/[p]/config.json" `isHardLinkToFile` "input/sp2-config.json"


    let abacInnerRules = do
        "supercells.json" `isCopiedFromFile` "input/ab/supercells.json"
        "assemble/spatial-params.toml" `isCopiedFromFile` "input/ab/spatial-params.toml"
        "assemble/layers.toml"         `isCopiedFromFile` "input/ab/layers.toml"

        let configRule lj = \path F{..} -> do
            Just supercell <- needsFile "supercells.json" >>= flip getJson ["phonopy"] . idgaf
            copyPath (file "config.json") path
            setJson (idgaf path) ["phonopy","supercell_dim"] supercell
            setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj

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

        "bandplot/input-data-vdw.dat"             `datIsConvertedFromYaml` "uncross/vdw/corrected.yaml"
        "bandplot/input-data-novdw.dat"           `datIsConvertedFromYaml` "uncross/novdw/corrected.yaml"
        "bandplot/input-data-[v]-orig.dat"           `datIsConvertedFromYaml` "uncross/[v]/eigenvalues.yaml"
        "bandplot/input-data-[v]-zpol.dat"           `datIsConvertedFromYaml` "zpol.[v]/out.yaml"
        "bandplot/input-data-perturb1.dat"        `datIsConvertedFromYaml` "perturb1/perturb1.yaml"
        "zpol.[v]/template.yaml" `isHardLinkToFile` "sp2.[v]/eigenvalues.yaml"

    enter "aba/[p]" $ abacInnerRules
    enter "abc/[p]" $ abacInnerRules


    "bandplot/gdm/two/input-data-abc-vdw.dat" `isHardLinkToFile` "bandplot/abc/abc/input-data-vdw.dat"
    "bandplot/gdm/two/input-data-aba-vdw.dat"  `isHardLinkToFile` "bandplot/abc/aba/input-data-vdw.dat"

    enter "aba/[p]" $ do

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


    enter "abc/[p]" $ do

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

    "bandplot/gdm/two/title" `isCopiedFromDir` "bandplot/abc/abc"
    "bandplot/gdm/two/band_xticks.txt" `isCopiedFromDir` "bandplot/abc/abc"
    "bandplot/gdm/two/data-prelude.dat" `isCopiedFromDir` "bandplot/abc/abc"


    "graphene/assemble"     `isDirectorySymlinkTo` "assemble/graphene/graphene"
    "graphene/ev-cache.[v]" `isDirectorySymlinkTo` "ev-cache/graphene/[v]"
    "graphene/sp2.[v]"            `isDirectorySymlinkTo`  "sp2/graphene/[v]"
    "graphene/uncross"      `isDirectorySymlinkTo` "uncross/graphene/graphene"
    "graphene/perturb1"     `isDirectorySymlinkTo` "perturb1/graphene/graphene"
    "graphene/fold.[v]"     `isDirectorySymlinkTo` "fold/graphene/[v]"
    "graphene/unfold.[v]"   `isDirectorySymlinkTo` "unfold/graphene/[v]"
    "graphene/bandplot"     `isDirectorySymlinkTo` "bandplot/graphene/graphene"
    "uncross/graphene/graphene/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/graphene/[v]"
    "perturb1/graphene/graphene/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/graphene/[v]"
    "zpol/graphene/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/graphene/[v]"

    "graphene/hsym.json"   `isCopiedFromFile` "input/hsym.json"
    "graphene/config.json" `isHardLinkToFile` "input/sp2-config.json"

    enter "graphene" $ do
        "assemble/spatial-params.toml" `isCopiedFromFile` "spatial-params.toml"
        "assemble/layers.toml"         `isCopiedFromFile` "layers.toml"

        let configRule lj = \path F{..} -> do
            Just supercell <- needsFile "supercells.json" >>= flip getJson ["phonopy"] . idgaf
            copyPath (file "config.json") path
            setJson (idgaf path) ["phonopy","supercell_dim"] supercell
            setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj

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
        "zpol.[v]/template.yaml" `isHardLinkToFile` "sp2.[v]/eigenvalues.yaml"

    "bandplot/graphene/graphene/input-data-vdw.dat"             `datIsConvertedFromYaml` "uncross/graphene/graphene/vdw/corrected.yaml"
    "bandplot/graphene/graphene/input-data-novdw.dat"           `datIsConvertedFromYaml` "uncross/graphene/graphene/novdw/corrected.yaml"
    "bandplot/graphene/graphene/input-data-perturb1.dat"        `datIsConvertedFromYaml` "perturb1/graphene/graphene/perturb1.yaml"
    "bandplot/graphene/graphene/title" !> \fp _ -> writePath fp "Graphene"
    "bandplot/graphene/graphene/band_xticks.txt"  `isCopiedFromDir` "bandplot/pat/001-a"
    "bandplot/graphene/graphene/data-prelude.dat" `isCopiedFromDir` "bandplot/pat/001-a"



    "4-layer/assemble"     `isDirectorySymlinkTo` "assemble/4-layer/4-layer"
    "4-layer/ev-cache.[v]" `isDirectorySymlinkTo` "ev-cache/4-layer/[v]"
    "4-layer/sp2.[v]"            `isDirectorySymlinkTo`  "sp2/4-layer/[v]"
    "4-layer/uncross"      `isDirectorySymlinkTo` "uncross/4-layer/4-layer"
    "4-layer/perturb1"     `isDirectorySymlinkTo` "perturb1/4-layer/4-layer"
    "4-layer/fold.[v]"     `isDirectorySymlinkTo` "fold/4-layer/[v]"
    "4-layer/unfold.[v]"   `isDirectorySymlinkTo` "unfold/4-layer/[v]"
    "4-layer/bandplot"     `isDirectorySymlinkTo` "bandplot/4-layer/4-layer"
    "4-layer/zpol.[v]"     `isDirectorySymlinkTo` "zpol/4-layer/[v]"
    "uncross/4-layer/4-layer/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/4-layer/[v]"
    "perturb1/4-layer/4-layer/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/4-layer/[v]"
    "zpol/4-layer/[v]/ev-cache" `isDirectorySymlinkTo` "ev-cache/4-layer/[v]"

    "4-layer/hsym.json"   `isCopiedFromFile` "input/hsym.json"
    "4-layer/config.json" `isHardLinkToFile` "input/sp2-config.json"

    enter "4-layer" $ do
        "assemble/spatial-params.toml" `isCopiedFromFile` "spatial-params.toml"
        "assemble/layers.toml"         `isCopiedFromFile` "layers.toml"

        let configRule lj = \path F{..} -> do
            Just supercell <- needsFile "supercells.json" >>= flip getJson ["phonopy"] . idgaf
            copyPath (file "config.json") path
            setJson (idgaf path) ["phonopy","supercell_dim"] supercell
            setJson (idgaf path) ["lammps","compute_lj"] $ Aeson.Bool lj

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
        "zpol.[v]/template.yaml" `isHardLinkToFile` "sp2.[v]/eigenvalues.yaml"

    "bandplot/4-layer/4-layer/input-data-vdw.dat"             `datIsConvertedFromYaml` "uncross/4-layer/4-layer/vdw/corrected.yaml"
    "bandplot/4-layer/4-layer/input-data-novdw.dat"           `datIsConvertedFromYaml` "uncross/4-layer/4-layer/novdw/corrected.yaml"
    "bandplot/4-layer/4-layer/input-data-vdw-orig.dat"        `datIsConvertedFromYaml` "uncross/4-layer/4-layer/vdw/eigenvalues.yaml"
    "bandplot/4-layer/4-layer/input-data-novdw-orig.dat"        `datIsConvertedFromYaml` "uncross/4-layer/4-layer/novdw/eigenvalues.yaml"
    "bandplot/4-layer/4-layer/input-data-vdw-zpol.dat"        `datIsConvertedFromYaml` "zpol/4-layer/vdw/out.yaml"
    "bandplot/4-layer/4-layer/input-data-novdw-zpol.dat"        `datIsConvertedFromYaml` "zpol/4-layer/novdw/out.yaml"
    "bandplot/4-layer/4-layer/input-data-perturb1.dat"        `datIsConvertedFromYaml` "perturb1/4-layer/4-layer/perturb1.yaml"
    "bandplot/4-layer/4-layer/title" !> \fp _ -> writePath fp "Four layers (ABAB)"
    "bandplot/4-layer/4-layer/band_xticks.txt"  `isCopiedFromDir` "bandplot/pat/001-a"
    "bandplot/4-layer/4-layer/data-prelude.dat" `isCopiedFromDir` "bandplot/pat/001-a"


plottingRules :: App ()
plottingRules = do
    -- HACK
    -- some parts of the plotting process look at a couple of pieces of sp2 output
    --  and I'm not yet up to untangling them.

    "bandplot/pat/[p]/band_labels.txt" `isCopiedFromFile` "sp2/pat/[p].novdw/band_labels.txt"

    "bandplot/[c]/[x]/prelude.gplot"          `isCopiedFromFile` "input/gplot-helper/prelude.gplot"
    "bandplot/[c]/[x]/write-band-[ext].gplot" `isCopiedFromFile` "input/gplot-helper/write-band-[ext].gplot"

    "bandplot/[c]/[x]/templates" `isDirectorySymlinkTo` "input/gplot-templates"

    enter "bandplot" $ do

        enter "[c]/[p]" $ do

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
                cols <- needsDataJson (file fp)
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
            "work-data-abac.dat" !> \dataOut F{..} -> do
                ([xs, ysN]) <- needsDataJson (file "input-data-abc-vdw.json")
                ([_,  ysV]) <- needsDataJson (file "input-data-aba-vdw.json")
                produceDat dataOut [xs, ysV, ysN]

            "work-data-both.dat" !> \dataOut F{..} -> do
                ([xs, ysN]) <- needsDataJson (file "input-data-novdw.json")
                ([_,  ysV]) <- needsDataJson (file "input-data-vdw.json")
                produceDat dataOut [xs, ysN, ysV]

            "work-data-all.dat" !> \dataOut F{..} -> do
                ([xs, ys0]) <- needsDataJson (file "input-data-novdw.json")
                ([_,  ysV]) <- needsDataJson (file "input-data-vdw.json")
                ([_,  ys1]) <- needsDataJson (file "input-data-perturb1.json")
                produceDat dataOut [xs, ys0, ysV, ys1]

            "work-data-[v]-zpol.dat" !> \dataOut F{..} -> do
                ([xs, ysE]) <- needsDataJson (file "input-data-[v]-orig.json")
                ([_,  ysZ]) <- needsDataJson (file "input-data-[v]-zpol.json")
                produceDat dataOut [xs, ysE, ysZ]

            "work-data-[v]-folded-ab.dat" !> \dataOut F{..} -> do
                ([xs, ys0]) <- needsDataJson (file "input-data-[v].json")
                ([_,  ys1]) <- needsDataJson (file "input-data-[v]-folded-ab.json")
                produceDat dataOut [xs, ys0, ys1]

            "work-data-[v]-unfolded-ab.dat" !> \dataOut F{..} -> do
                ([ _, ys0']) <- needsDataJson (file "input-data-[v]-perfect-ab.json")
                ([xs, ys1 ]) <- needsDataJson (file "input-data-[v]-unfolded-ab.json")
                numDupes <- patternVolume (fmt "[p]") -- FIXME [p], only for pat
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

            "plot-data-abac.dat"            `isHardLinkToFile` "work-data-abac.dat"
            "plot-data-novdw.dat"           `isHardLinkToFile` "input-data-novdw.dat"
            "plot-data-vdw.dat"             `isHardLinkToFile` "input-data-vdw.dat"
            "plot-data-both.dat"            `isHardLinkToFile` "work-data-both.dat"
            "plot-data-perturb1.dat"        `isHardLinkToFile` "work-data-all.dat"
            "plot-data-filter.dat"          `isHardLinkToFile` "work-data-filter.dat"
            "plot-data-[v]-zpol.dat"        `isHardLinkToFile` "work-data-[v]-zpol.dat"
            "plot-data-[v]-xypol.dat"       `isHardLinkToFile` "work-data-[v]-zpol.dat"
            "plot-data-[v]-xypol-zoom.dat"  `isHardLinkToFile` "work-data-[v]-zpol.dat"
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
        enter "[c]/[x]" $ do
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
        enter "[c]/[x]" $ do
            "vdw.gplot.template"                  `isCopiedFromFile` "templates/band.gplot.template"
            "novdw.gplot.template"                `isCopiedFromFile` "templates/band.gplot.template"
            "both.gplot.template"                 `isCopiedFromFile` "templates/both.gplot.template"
            "filter.gplot.template"               `isCopiedFromFile` "templates/filter.gplot.template"
            "[v]-zpol.gplot.template"             `isCopiedFromFile` "templates/zpol.gplot.template"
            "[v]-xypol.gplot.template"            `isCopiedFromFile` "templates/xypol.gplot.template"
            "[v]-xypol-zoom.gplot.template"       `isCopiedFromFile` "templates/xypol-zoom.gplot.template"
            "[v]-folded-ab.gplot.template"        `isCopiedFromFile` "templates/folded.gplot.template"
            "[v]-unfolded-ab.gplot.template"      `isCopiedFromFile` "templates/folded.gplot.template"
            "[v]-folded-ab-filter.gplot.template" `isCopiedFromFile` "templates/folded-filter.gplot.template"
            "perturb1.gplot.template"             `isCopiedFromFile` "templates/perturb1.gplot.template"
            "num-[i].gplot.template"              `isCopiedFromFile` "templates/both.gplot.template"
            "abac.gplot.template"                 `isCopiedFromFile` "templates/both.gplot.template"

    enter "bandplot" $
        enter "[c]/[p]" $ do

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
    "out/bands/[c]/[p]_[s].[ext]" `isCopiedFromFile` "bandplot/[c]/[p]/plot_[s].[ext]"


-- gnuplot data is preparsed into JSON, which can be parsed much
--   faster by any python scripts that we still use.
-- (and infinitely easier to work with in the python repl)
degnuplot :: PartialCmd
degnuplot = script "degnuplot -Gbhkx -Jxhkb"
engnuplot :: PartialCmd
engnuplot = script "engnuplot -Gbhkx -Jxhkb"

-- this awkward pattern pops up every now and then in my code and it never looked very readable to me
--  except in the most trivial cases
readModifyWrite :: (Monad m)=> (a -> b) -> m a -> (b -> m c) -> m c
readModifyWrite f read write = f <$> read >>= write

------------------------------------------------------------

-- Operator to create a band.yaml -> data.dat rule.
datIsConvertedFromYaml :: Pat -> Pat -> App ()
datIsConvertedFromYaml dataPat yamlPat =
    isolate
        [ Produces dataPat (From "out.dat")
        , Requires yamlPat (As "band.yaml")
        ] $ \tmp _ ->
            liftAction $ cmd "bandplot --gnuplot" (Cwd tmp) (FileStdout (tmp </> "out.dat"))

-- relocated implementation of a function for dealing with plot data
filterShiftedOnColumnsImpl :: _ -- deal with it, ghc
filterShiftedOnColumnsImpl i1 i2 fp = \dataOut F{..} -> do

    cols <- needsDataJson (file fp)

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

-- HACK: tied to input file structure
-- HACK: shouldn't assume ab
patternVolume :: FileString -> Act Int
patternVolume p = do
    result <- maybe undefined id <$> needJSON ("input/pat" </> p </> "ab/positions.json")
    pure . maybe undefined id . flip Aeson.parseMaybe result $
        (Aeson..: "meta") >=> (Aeson..: "volume") >=> (aesonIndex 0)


--    "//irreps-*.yaml" !> \dest [stem, kpoint] -> runPhonopyIrreps stem kpoint dest

script :: FileString -> PartialCmd
script x = cmd ("scripts" </> x)

needsDataJson :: FileString -> Act [BandGplColumn]
needsDataJson = needJSON
