import           Test.Tasty(defaultMain)
import           TestUtils
import qualified Band.FoldTest as Fold

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = Fold.testSuite
