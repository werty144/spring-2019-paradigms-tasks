import Test.Tasty

import TestBasics
import TestRobots

main :: IO ()
main = defaultMain $ testGroup "All tests" [testsBasics, testsRobots]
