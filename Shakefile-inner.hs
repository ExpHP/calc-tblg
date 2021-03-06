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
import qualified "text" Data.Text as Text
import qualified "text" Data.Text.IO as Text
import qualified "vector" Data.Vector as Vector
import qualified "containers" Data.Map as Map
import qualified "aeson" Data.Aeson as Aeson
import qualified "aeson" Data.Aeson.Types as Aeson
import           "turtle-eggshell" Eggshell hiding (need,view,empty,(</>))

-- import qualified Turtle.Please as Turtle hiding (empty)
import           Rules.Comp(componentRules, GoldenSearchConfig(..))
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
    -- , shakeVerbosity = Normal
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
    enter "comp" $ componentRules
    plottingRules

-------------------------------------------

mainRules :: App ()
mainRules = do

    -- HACK
    "comp/sp2/[c]/[x]/mutate.py" `isCopiedFromFile` "input/mutate.py"

    let perfectABPattern = "001-b"

    ------------------------------
    -- The work/ subtree is where we put everything together.
    -- This is where we define which instances actually exist of the various components,
    --  and hook up all of their inputs and outputs.
    --
    -- First, we define instances of the component computations by creating symlinks.
    -- (this MUST be done first)

    "proj/pat/[p]/pat"          `isDirectorySymlinkTo` "input/pat/[p]"

    "proj/pat/[p]/improve-layer-sep.[v]" `isDirectorySymlinkTo` "comp/improve-layer-sep/pat/[p].[v]"
    "proj/pat/[p]/improve-param-a.[v]" `isDirectorySymlinkTo` "comp/improve-param-a/pat/[p].[v]"
    "proj/pat/[p]/assemble.[v]" `isDirectorySymlinkTo` "comp/assemble/pat/[p].[v]"
    "proj/pat/[p]/ev-cache.[v]" `isDirectorySymlinkTo` "comp/ev-cache/pat/[p].[v]"
    "proj/pat/[p]/sp2.[v]"            `isDirectorySymlinkTo`  "comp/sp2/pat/[p].[v]"
    "proj/pat/[p]/perfect-ab-sp2.[v]" `isDirectorySymlinkTo` ("comp/sp2/pat/" ++ perfectABPattern ++ ".[v]") -- for folding
    "proj/pat/[p]/uncross"      `isDirectorySymlinkTo` "comp/uncross/pat/[p]"
    "proj/pat/[p]/perturb1"     `isDirectorySymlinkTo` "comp/perturb1/pat/[p]"
    "proj/pat/[p]/fold.[v]"     `isDirectorySymlinkTo` "comp/fold/pat/[p].[v]"
    "proj/pat/[p]/unfold.[v]"   `isDirectorySymlinkTo` "comp/unfold/pat/[p].[v]"
    "proj/pat/[p]/post"     `isDirectorySymlinkTo` "post/pat/[p]"
    "proj/pat/[p]/zpol.[v]"     `isDirectorySymlinkTo` "comp/zpol/pat/[p].[v]"
    "proj/pat/[p]/xypol.[v]"    `isDirectorySymlinkTo` "comp/xypol/pat/[p].[v]"
    "comp/uncross/pat/[p]/[v]/ev-cache"  `isDirectorySymlinkTo` "comp/ev-cache/pat/[p].[v]/"
    "comp/perturb1/pat/[p]/[v]/ev-cache" `isDirectorySymlinkTo` "comp/ev-cache/pat/[p].[v]/"
    "comp/zpol/pat/[p].[v]/ev-cache"     `isDirectorySymlinkTo` "comp/ev-cache/pat/[p].[v]/"

    "proj/pat/[p]/hsym.json"   `isCopiedFromFile` "input/hsym.json"
    "proj/pat/[p]/config.json" `isCopiedFromFile` "input/sp2-config.json"
    "proj/pat/[p]/golden-search-original.yaml" `isCopiedFromFile` "input/golden-search.yaml"
    "proj/pat/[p]/layer-linear-search.toml"    `isCopiedFromFile` "input/layer-linear-search.toml"
    "proj/pat/[p]/param-a-linear-search.toml"  `isCopiedFromFile` "input/param-a-linear-search.toml"

    let configRule baseConfig supercellsJson lj = \outConfig F{..} -> do
        Just supercell <- needsFile supercellsJson >>= flip getJson ["phonopy"] . idgaf
        copyPath (file baseConfig) outConfig
        setJson (idgaf outConfig) ["phonopy","supercell_dim"] supercell
        setJson (idgaf outConfig) ["lammps","compute_lj"] $ Aeson.Bool lj

    ------------------------------
    -- Now, we can hook up all of the input and output files.
    -- We can freely use our symlinks; there are mechanisms built into our 'App' monad
    --  which ensure that the dependency tree we present to Shake has the correct structure.

    enter "proj/pat/[p]" $ do

        -- FIXME exorcise the ab subdirs
        "positions.json"               `isLinkedFromFile` "pat/positions.json"

        "supercells.json"              `isCopiedFromFile` "pat/supercells.json"
        "spatial-params-original.yaml" `isCopiedFromFile` "pat/spatial-params.yaml"
        "layers.yaml"                  `isCopiedFromFile` "pat/layers.yaml"
        "assemble.[v]/spatial-params.yaml" `isCopiedFromFile` "spatial-params-improved-[v].yaml"
        "assemble.[v]/layers.yaml"         `isCopiedFromFile` "layers.yaml"

        -- linear searches to improve initial guess

        "improve-layer-sep.[v]/sp2-config.json"        `isCopiedFromFile` "sp2.[v]/config.json"
        "improve-layer-sep.[v]/spatial-params-in.yaml" `isCopiedFromFile` "spatial-params-original.yaml"
        "improve-layer-sep.[v]/layers.yaml"            `isCopiedFromFile` "layers.yaml"
        "improve-layer-sep.[v]/supercells.json"        `isCopiedFromFile` "supercells.json"
        "improve-layer-sep.[v]/linear-search.toml"     `isCopiedFromFile` "layer-linear-search.toml"

        "spatial-params-improved-[v].yaml" `isCopiedFromFile` "improve-layer-sep.[v]/spatial-params-out.yaml"

        "improve-param-a.[v]/sp2-config.json"        `isCopiedFromFile` "sp2.[v]/config.json"
        "improve-param-a.[v]/spatial-params-in.yaml" `isCopiedFromFile` "spatial-params-improved-[v].yaml"
        "improve-param-a.[v]/layers.yaml"            `isCopiedFromFile` "layers.yaml"
        "improve-param-a.[v]/supercells.json"        `isCopiedFromFile` "supercells.json"
        "improve-param-a.[v]/linear-search.toml"     `isCopiedFromFile` "param-a-linear-search.toml"

        "golden-search-improved-[v].yaml" %> goldenSearchRule "golden-search-original.yaml"
                                                              "improve-param-a.[v]/min-out"
                                                              "improve-param-a.[v]/max-out"

        "sp2.vdw/config.json"   !> configRule "config.json" "supercells.json" True
        "sp2.novdw/config.json" !> configRule "config.json" "supercells.json" False

        "sp2.[v]/moire.vasp" `isLinkedFromFile` "assemble.[v]/moire.vasp"
        "sp2.[v]/golden-search.yaml" `isCopiedFromFile` "golden-search-improved-[v].yaml"

        -- NOTE: total match
        "ev-cache.[v]/force_constants.hdf5" `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/FORCE_SETS"           `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/sc.conf"              `isCopiedFromDir` "sp2.[v]"
        "ev-cache.[v]/relaxed.vasp"         `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/hsym.json"            `isCopiedFromFile` "hsym.json"
        "uncross/hsym.json"             `isCopiedFromFile` "hsym.json"
        "uncross/[v]/eigenvalues.yaml"  `isLinkedFromFile` "sp2.[v]/eigenvalues.yaml"
        "perturb1/[v]/eigenvalues.yaml" `isLinkedFromFile` "sp2.[v]/eigenvalues.yaml"

        -- FIXME folding doesn't make much sense now that we relax params
        --       (vdw and non-vdw lattice may differ)
        "fold.[v]/template.yaml"            `isLinkedFromFile` "sp2.[v]/eigenvalues.yaml"
        "fold.[v]/coeffs.json"              `isCopiedFromFile` "coeffs.json"
        "fold.[v]/hsym.json"                `isCopiedFromFile` "hsym.json"
        "fold.[v]/sub/structure.vasp"       `isLinkedFromFile` "perfect-ab-sp2.[v]/relaxed.vasp"
        "fold.[v]/sub/force_constants.hdf5" `isLinkedFromFile` "perfect-ab-sp2.[v]/force_constants.hdf5"
        "fold.[v]/sub/sc.conf"              `isCopiedFromFile` "perfect-ab-sp2.[v]/sc.conf"

        "unfold.[v]/template.yaml"              `isLinkedFromFile` "sp2.[v]/eigenvalues.yaml"
        "unfold.[v]/coeffs.json"                `isCopiedFromFile` "coeffs.json"
        "unfold.[v]/hsym.json"                  `isCopiedFromFile` "hsym.json"
        "unfold.[v]/super/structure.vasp"       `isLinkedFromFile` "sp2.[v]/relaxed.vasp"
        "unfold.[v]/super/force_constants.hdf5" `isLinkedFromFile` "sp2.[v]/force_constants.hdf5"
        "unfold.[v]/super/sc.conf"              `isCopiedFromFile` "sp2.[v]/sc.conf"

        "post/input-data-vdw.dat"             `datIsConvertedFromYaml` "uncross/vdw/corrected.yaml"
        "post/input-data-novdw.dat"           `datIsConvertedFromYaml` "uncross/novdw/corrected.yaml"
        "post/input-data-[v]-orig.dat"        `datIsConvertedFromYaml` "uncross/[v]/eigenvalues.yaml"
        "post/input-data-[v]-zpol.dat"        `datIsConvertedFromYaml` "zpol.[v]/out.yaml"
        "post/input-data-perturb1.dat"        `datIsConvertedFromYaml` "perturb1/perturb1.yaml"

        "post/input-data-[v]-folded-ab.dat"   `datIsConvertedFromYaml` "fold.[v]/out.yaml"
        "post/input-data-[v]-unfolded-ab.dat" `datIsConvertedFromYaml` "unfold.[v]/out.yaml"
        "post/input-data-[v]-perfect-ab.dat"  `datIsConvertedFromYaml` "perfect-ab-sp2.[v]/eigenvalues.yaml"

    -- FIXME doesn't seem to register for mysterious reasons
    -- "zpol.[v]/template.yaml"              `isCopiedFromFile` "sp2.[v]/eigenvalues.yaml"
    "comp/zpol/pat/[p].[v]/template.yaml"              `isCopiedFromFile` "comp/sp2/pat/[p].[v]/eigenvalues.yaml"

    -- a final couple of awkward bits and pieces of data needed by 'bandplot/'
    enter "proj/pat/[p]" $ do
        "coeffs.json" %> \F{..} -> do
            result <- maybe undefined id <$> needJsonFile "positions.json"
            mat <- either fail pure . flip Aeson.parseEither result $
                (Aeson..: "meta") >=> (Aeson..: "coeff") >=> (aesonIndex 0)
            pure (mat :: [[Int]])

        "post/title" !> extractLinesFromFile 1 "sp2.novdw/moire.vasp"
        "band_labels.txt" `isCopiedFromFile` "sp2.novdw/band_labels.txt"
        "post/band_xticks.txt" !> bandXTicksRuleOnFiles "data-prelude.dat" "band_labels.txt"

        -- files created by datIsConvertedFromYaml` have a short prelude containing xtick positions
        "data-prelude.dat" !> extractLinesFromFile 3 "post/input-data-vdw.dat"

    ---------------------------------------------

    -- (this could not wait for a reasonable abstraction to have a pattern
    --  like [a] iterate over a set of specific values)
    -- NOTE: including ab here is a HACK because the old proj/pat doesn't
    --        do the lattice parameters and it looks ugly to copy over
    forM [ "aba", "abc" ] $ \abStr -> do
        let rpl ('#':'#':s) = abStr ++ rpl s
            rpl (c:s) = c:rpl s
            rpl [] = []

        rpl "proj/##/[p]/input"          `isDirectorySymlinkTo` rpl "input/##-rot/[p]"

        rpl "proj/##/[p]/improve-layer-sep.[v]" `isDirectorySymlinkTo` rpl "comp/improve-layer-sep/##/[p].[v]"
        rpl "proj/##/[p]/improve-param-a.[v]" `isDirectorySymlinkTo` rpl "comp/improve-param-a/##/[p].[v]"
        rpl "proj/##/[p]/assemble.[v]"     `isDirectorySymlinkTo` rpl "comp/assemble/##/[p].[v]"
        rpl "proj/##/[p]/ev-cache.[v]" `isDirectorySymlinkTo` rpl "comp/ev-cache/##/[p].[v]"
        rpl "proj/##/[p]/sp2.[v]"      `isDirectorySymlinkTo` rpl "comp/sp2/##/[p].[v]"
        rpl "proj/##/[p]/uncross"      `isDirectorySymlinkTo` rpl "comp/uncross/##/[p]"
        rpl "proj/##/[p]/perturb1"     `isDirectorySymlinkTo` rpl "comp/perturb1/##/[p]"
        rpl "comp/uncross/##/[p]/[v]/ev-cache"  `isDirectorySymlinkTo` rpl "comp/ev-cache/##/[p].[v]/"
        rpl "comp/perturb1/##/[p]/[v]/ev-cache" `isDirectorySymlinkTo` rpl "comp/ev-cache/##/[p].[v]/"
        rpl "proj/##/[p]/post"     `isDirectorySymlinkTo` rpl "post/##/[p]"

        rpl "proj/##/[p]/zpol.[v]"     `isDirectorySymlinkTo` rpl "comp/zpol/##/[p].[v]"
        rpl "comp/zpol/##/[p].[v]/ev-cache" `isDirectorySymlinkTo` rpl "comp/ev-cache/##/[p].[v]"

        rpl "proj/##/[p]/hsym.json"   `isCopiedFromFile` rpl "input/hsym.json"

        rpl "proj/##/[p]/config.json" `isCopiedFromFile` rpl "input/sp2-config.json"

        rpl "proj/##/[p]/golden-search-original.yaml" `isCopiedFromFile` rpl "input/golden-search.yaml"
        rpl "proj/##/[p]/layer-linear-search.toml" `isCopiedFromFile` rpl "input/layer-linear-search.toml"
        rpl "proj/##/[p]/param-a-linear-search.toml" `isCopiedFromFile` rpl "input/param-a-linear-search.toml"

    let abacInnerRules = do
        "spatial-params-original.yaml" `isCopiedFromFile` "input/spatial-params.yaml"
        "layers.yaml"                  `isCopiedFromFile` "input/layers.yaml"
        "supercells.json"              `isCopiedFromFile` "input/supercells.json"

        -- linear searches to improve initial guess

        "improve-layer-sep.[v]/sp2-config.json"        `isCopiedFromFile` "sp2.[v]/config.json"
        "improve-layer-sep.[v]/spatial-params-in.yaml" `isCopiedFromFile` "spatial-params-original.yaml"
        "improve-layer-sep.[v]/layers.yaml"            `isCopiedFromFile` "layers.yaml"
        "improve-layer-sep.[v]/supercells.json"        `isCopiedFromFile` "supercells.json"
        "improve-layer-sep.[v]/linear-search.toml"     `isCopiedFromFile` "layer-linear-search.toml"

        "spatial-params-improved-[v].yaml" `isCopiedFromFile` "improve-layer-sep.[v]/spatial-params-out.yaml"

        "improve-param-a.[v]/sp2-config.json"        `isCopiedFromFile` "sp2.[v]/config.json"
        "improve-param-a.[v]/spatial-params-in.yaml" `isCopiedFromFile` "spatial-params-improved-[v].yaml"
        "improve-param-a.[v]/layers.yaml"            `isCopiedFromFile` "layers.yaml"
        "improve-param-a.[v]/supercells.json"        `isCopiedFromFile` "supercells.json"
        "improve-param-a.[v]/linear-search.toml"     `isCopiedFromFile` "param-a-linear-search.toml"

        "golden-search-improved-[v].yaml" %> goldenSearchRule "golden-search-original.yaml"
                                                              "improve-param-a.[v]/min-out"
                                                              "improve-param-a.[v]/max-out"

        "assemble.[v]/spatial-params.yaml" `isCopiedFromFile` "spatial-params-improved-[v].yaml"
        "assemble.[v]/layers.yaml"         `isCopiedFromFile` "layers.yaml"

        "sp2.vdw/config.json"   !> configRule "config.json" "supercells.json" True
        "sp2.novdw/config.json" !> configRule "config.json" "supercells.json" False
        "sp2.[v]/golden-search.yaml" `isCopiedFromFile` "golden-search-improved-[v].yaml"
        "sp2.[v]/moire.vasp" `isLinkedFromFile` "assemble.[v]/moire.vasp"

        "ev-cache.[v]/force_constants.hdf5" `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/FORCE_SETS"           `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/sc.conf"              `isCopiedFromDir` "sp2.[v]"
        "ev-cache.[v]/relaxed.vasp"         `isLinkedFromDir` "sp2.[v]"
        "ev-cache.[v]/hsym.json"            `isCopiedFromFile` "hsym.json"
        "uncross/hsym.json"             `isCopiedFromFile` "hsym.json"
        "uncross/[v]/eigenvalues.yaml"  `isLinkedFromFile` "sp2.[v]/eigenvalues.yaml"
        "perturb1/[v]/eigenvalues.yaml" `isLinkedFromFile` "sp2.[v]/eigenvalues.yaml"

        "post/input-data-vdw.dat"             `datIsConvertedFromYaml` "uncross/vdw/corrected.yaml"
        "post/input-data-novdw.dat"           `datIsConvertedFromYaml` "uncross/novdw/corrected.yaml"
        "post/input-data-[v]-orig.dat"           `datIsConvertedFromYaml` "uncross/[v]/eigenvalues.yaml"
        "post/input-data-[v]-zpol.dat"           `datIsConvertedFromYaml` "zpol.[v]/out.yaml"
        "post/input-data-perturb1.dat"        `datIsConvertedFromYaml` "perturb1/perturb1.yaml"
        "zpol.[v]/template.yaml" `isLinkedFromFile` "sp2.[v]/eigenvalues.yaml"

        "post/title" !> extractLinesFromFile 1 "sp2.novdw/moire.vasp"

        "band_labels.txt" `isCopiedFromFile` "sp2.novdw/band_labels.txt"
        "post/band_xticks.txt" !> bandXTicksRuleOnFiles "data-prelude.dat" "band_labels.txt"
        "data-prelude.dat" !> extractLinesFromFile 3 "post/input-data-vdw.dat"


    enter "proj/aba/[p]" $ abacInnerRules
    enter "proj/abc/[p]" $ abacInnerRules
    enter "proj/ab/[p]" $ abacInnerRules


    -- ???!???
    "post/gdm/two/input-data-abc-vdw.dat" `isLinkedFromFile` "post/abc/abc/input-data-vdw.dat"
    "post/gdm/two/input-data-aba-vdw.dat" `isLinkedFromFile` "post/abc/aba/input-data-vdw.dat"

    "post/gdm/two/title" `isCopiedFromDir` "post/abc/abc"
    "post/gdm/two/band_xticks.txt" `isCopiedFromDir` "post/abc/abc"
    "post/gdm/two/data-prelude.dat" `isCopiedFromDir` "post/abc/abc"

    -------------------------------------------

    thereExistsFile "proj/params/[p]/layers.yaml"
    thereExistsFile "proj/params/[p]/spatial-params.yaml"
    thereExistsFile "proj/params/[p]/supercells.json"

    "proj/params/[p]/assemble/[a]-[c]" `isDirectorySymlinkTo` "comp/assemble/params/[p]_[a]-[c]"
    "proj/params/[p]/sp2.[v]/[a]-[c]"  `isDirectorySymlinkTo` "comp/sp2/params/[p].[v]_[a]-[c]"
    "proj/params/[p]/hsym.json"    `isCopiedFromFile`     "input/hsym.json"

    enter "proj/params/[p]" $ do
        -- for easier identification from bash
        "sp2.[v]/[a]-[c]/a.txt" !> \fp F{..} -> writePath fp (fmt "[a]\n")
        "sp2.[v]/[a]-[c]/c.txt" !> \fp F{..} -> writePath fp (fmt "[c]\n")
        "sp2.[v]/[a]-[c]/golden-search.yaml" %> \F{..} -> pure NoGoldenSearch

        "assemble/[a]-[c]/spatial-params.yaml" !> \outYaml F{..} -> do
            map <- needsFile "spatial-params.yaml" >>= readYaml :: Act (Map String Double)

            let map' = Map.adjust (* read (fmt "[a]")) "a"
                     $ Map.adjust (* read (fmt "[c]")) "layer-sep"
                     $ map
            writeYaml outYaml map'

        "assemble/[a]-[c]/layers.yaml" `isCopiedFromFile` "layers.yaml"

        "sp2.vdw/[a]-[c]/config.json"   !> configRule "config.json" "supercells.json" True
        "sp2.novdw/[a]-[c]/config.json" !> configRule "config.json" "supercells.json" False
        "sp2.[v]/[a]-[c]/moire.vasp" `isLinkedFromFile` "assemble/[a]-[c]/moire.vasp"



plottingRules :: App ()
plottingRules = do
    -- HACK
    -- some parts of the plotting process look at a couple of pieces of sp2 output
    --  and I'm not yet up to untangling them.

    "post/pat/[p]/band_labels.txt" `isCopiedFromFile` "sp2/pat/[p].novdw/band_labels.txt"

    "post/[c]/[p]/templates"  `isDirectorySymlinkTo` "input/gplot-templates"
    "comp/input/gplot-helper" `isDirectorySymlinkTo` "input/gplot-helper"

    enter "post" $ do

        -- NOTE: these tend to follow a different namespacing convention from components,
        --       as hinted at by the use of [p] instead of [x]
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
            "[j]-data-[k].json" !> \json F{..} -> do
                dat <- needsFile "[j]-data-[k].dat"
                liftAction $ degnuplot (FileStdin dat) (FileStdout json)

            --------------------------
            -- plot-data files
            -- There should be one for each .gplot.template rule
            -- FIXME these should perhaps be defined nearer those!

            "plot-data-abac.dat"            `isLinkedFromFile` "work-data-abac.dat"
            "plot-data-novdw.dat"           `isLinkedFromFile` "input-data-novdw.dat"
            "plot-data-vdw.dat"             `isLinkedFromFile` "input-data-vdw.dat"
            "plot-data-both.dat"            `isLinkedFromFile` "work-data-both.dat"
            "plot-data-perturb1.dat"        `isLinkedFromFile` "work-data-all.dat"
            "plot-data-filter.dat"          `isLinkedFromFile` "work-data-filter.dat"
            "plot-data-[v]-zpol.dat"        `isLinkedFromFile` "work-data-[v]-zpol.dat"
            "plot-data-[v]-xypol.dat"       `isLinkedFromFile` "work-data-[v]-zpol.dat"
            "plot-data-[v]-xypol-zoom.dat"  `isLinkedFromFile` "work-data-[v]-zpol.dat"
            "plot-data-[v]-folded-ab.dat"   `isLinkedFromFile` "work-data-[v]-folded-ab.dat"
            "plot-data-[v]-unfolded-ab.dat" `isLinkedFromFile` "work-data-[v]-unfolded-ab.dat"

            "plot-data-[v]-folded-ab-filter.dat" !>
                multiplexDirectly [ ("NaturalFilt", "work-data-filter-just-[v].dat")
                                  , ("NaturalAll",  "input-data-[v].dat")
                                  , ("Folded",      "input-data-[v]-folded-ab.dat")
                                  ]

            -- individual bands, for gauging the quality of the uncrosser
            "plot-data-num-[i].dat" !> \dataOut F{..} -> do
                (xs, ysN) <- needJsonFile "input-data-novdw.json" :: Act ([[[Double]]], [[[Double]]])
                (_,  ysV) <- needJsonFile "input-data-vdw.json"   :: Act ([[[Double]]], [[[Double]]])
                let extractBandI = fmap (List.transpose . (:[]) . (!! read (fmt "[i]")) . List.transpose)
                produceDat dataOut $ extractBandI <$> [xs, ysN, ysV]

    enter "post" $ do
        enter "[c]/[p]" $ do
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

    -- HACK: This mapping just helps get things working as before.
    --       Looking forward, however, it is overly restrictive, as we're imposing a part
    --        of the namespacing scheme onto all instances of bandplot.
    "comp/bandplot/[c]/[p]_[name]/data.dat"    `isLinkedFromFile` "post/[c]/[p]/plot-data-[name].dat"
    "comp/bandplot/[c]/[p]_[name]/xbase.gplot" `isCopiedFromFile` "post/[c]/[p]/xbase.gplot"

    "comp/bandplot/[c]/templates" `isDirectorySymlinkTo` "input/gplot-templates"
    enter "comp/bandplot" $ do
        enter "[c]" $ do
            "[p]_vdw/main.gplot"                  `isCopiedFromFile` "templates/band.gplot.template"
            "[p]_novdw/main.gplot"                `isCopiedFromFile` "templates/band.gplot.template"
            "[p]_both/main.gplot"                 `isCopiedFromFile` "templates/both.gplot.template"
            "[p]_filter/main.gplot"               `isCopiedFromFile` "templates/filter.gplot.template"
            "[p]_[v]-zpol/main.gplot"             `isCopiedFromFile` "templates/zpol.gplot.template"
            "[p]_[v]-xypol/main.gplot"            `isCopiedFromFile` "templates/xypol.gplot.template"
            "[p]_[v]-xypol-zoom/main.gplot"       `isCopiedFromFile` "templates/xypol-zoom.gplot.template"
            "[p]_[v]-folded-ab/main.gplot"        `isCopiedFromFile` "templates/folded.gplot.template"
            "[p]_[v]-unfolded-ab/main.gplot"      `isCopiedFromFile` "templates/folded.gplot.template"
            "[p]_[v]-folded-ab-filter/main.gplot" `isCopiedFromFile` "templates/folded-filter.gplot.template"
            "[p]_perturb1/main.gplot"             `isCopiedFromFile` "templates/perturb1.gplot.template"
            "[p]_num-[i]/main.gplot"              `isCopiedFromFile` "templates/both.gplot.template"
            "[p]_abac/main.gplot"                 `isCopiedFromFile` "templates/both.gplot.template"

    -- "out/" for collecting output across all patterns
    "out/bands/[c]/[x].[ext]" `isCopiedFromFile` "comp/bandplot/[c]/[x]/band.[ext]"


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
-- things that got copied and pasted around too much;
-- I had to factor them out to help see the differences between
--  two copied blocks. (THIS IS WHY YOU DON'T COPY AND PASTE)

bandXTicksRuleOnFiles :: Pat -> Pat -> _
bandXTicksRuleOnFiles dataPreludeDat bandLabelsTxt xvalsTxt F{..} = do
    -- third line has x positions.  First character is '#'.
    dataLines <- readLines (file dataPreludeDat)
    let counts = List.words . tail $ idgaf (dataLines !! 2)
    labels <- List.words <$> readPath (file bandLabelsTxt)

    let dquote = \s -> "\"" ++ s ++ "\""
    let paren  = \s -> "("  ++ s ++ ")"
    writePath xvalsTxt
        ("set xtics " ++ paren
            (List.intercalate ", "
                (List.zipWith (\l a -> dquote l ++ " " ++ a)
                    labels counts)))

extractLinesFromFile :: Int -> Pat -> _
extractLinesFromFile n input = \output F{..} ->
    readModifyWrite (take n) (readLines (file input))
                             (writeLines output)

-- NOTE: used with %>
goldenSearchRule :: Pat -> Pat -> Pat -> _
goldenSearchRule gsYaml minIn maxIn = \F{..} -> do
    orig <- needsFile gsYaml >>= readYaml
    lower <- read . idgaf <$> (needsFile minIn >>= liftIO . readFile)
    upper <- read . idgaf <$> (needsFile maxIn >>= liftIO . readFile)
    pure $ DoGoldenSearch
        { goldenSearchCfgLower = lower
        , goldenSearchCfgUpper = upper
        , goldenSearchCfgTol    = goldenSearchCfgTol orig
        , goldenSearchCfgMemory = goldenSearchCfgMemory orig
        }

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
    result <- maybe undefined id <$> needJson ("input/pat" </> p </> "positions.json")
    pure . maybe undefined id . flip Aeson.parseMaybe result $
        (Aeson..: "meta") >=> (Aeson..: "volume") >=> (aesonIndex 0)


--    "//irreps-*.yaml" !> \dest [stem, kpoint] -> runPhonopyIrreps stem kpoint dest

script :: FileString -> PartialCmd
script x = cmd ("scripts" </> x)

needsDataJson :: FileString -> Act [BandGplColumn]
needsDataJson = needJson
