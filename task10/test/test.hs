import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import Part1
import Part2

main = defaultMain tests

tests = testGroup "All tests" [basicsTests, robotsTests]

partOneTests = testGroup "Unit tests for Basics part"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [2,4..10]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "quickSort actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]
    ]

partTwoTests = let 
    walter = robot "Walter" 50 50
    testGroup "Unit tests for Robots part" 
    [testCase "Meet Walter!" $
        getName walter @?= "Walter"
    ]
