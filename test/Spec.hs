{-# OPTIONS_GHC -Wall -Werror #-}

import           Test.Tasty(defaultMain)
import           TestUtil
--  (Generate names with m-x spacemacs/uuidgen-4)
import           Band.Fold  (testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96)
import           GeneralUtil(testSuite_c2b7f613_b9d9_46ee_a252_78f5644ade15)
import           PathUtil(testSuite_0d4f6c7d_e418_4f21_be78_22d0210becbc)

main :: IO ()
main = defaultMain testSuite

--------------
-- This project embeds its tests in the source.
-- This enables the testing of private components, significantly lowering the amount
--  of friction against adding tests in general, with some caveats:
--
-- 1. testing-related utils may spill into the global namespace for a lot of code
--     that doesn't need them
-- 2. what would normally be test deps are now package deps
-- 3. it is still highly dangerous to put tests in modules with implicit export lists
--    (again, due to lack of warnings)
--
-- I dearly miss Rust's `#[cfg(test)] mod tests { ... }`, but what can ya do.
--
--------------
-- To make sure that all testSuites are linked into the root test tree, one can
--  probably compare the number of matches to these regexes:
--
-- Definitions (anywhere):   ^testSuite_.+ ::
-- Usages  (in this file):   ^    , testSuite_
--------------

testSuite :: TestTree
testSuite = "All Tests" ~:
    [ "(dummy)" ~:# ([] :: [[TestTree]]) -- let all lines use commas for easier grepping
    , testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96
    , testSuite_c2b7f613_b9d9_46ee_a252_78f5644ade15
    , testSuite_0d4f6c7d_e418_4f21_be78_22d0210becbc
    ]
