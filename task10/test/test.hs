import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Part1
import Part2

main = defaultMain unitTests

unitTests = testGroup "Unit Tests"
    [
        testCase "Arithemitcs is not fucked" $
            1 + 1 == 2 @?= True
    ]
