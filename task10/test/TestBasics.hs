import Test.Tasty
import Test.Tasty.HUnit

import Basics

main :: IO ()
main = defaultMain testsBasics

testsBasics :: TestTree
testsBasics = testGroup "Unit tests for Basics tasks"
    [testCase "head' works on non-empty list" $
        head' [1,2,3] @?= 1

    , testCase "head' works on infinite list" $
        head' [1,3..] @?= 1

    , testCase "tail' works on non-empty list too" $
        tail' [1,2,3] @?= [2,3]

    , testCase "tail' works on empty list" $
        tail' ([] :: [Integer]) @?= []

    , testCase "tail' works on infinite list" $
        take' 3 (tail' [1..]) @?=  [2,3,4]

    , testCase "take' takes 1 element from 3-element list" $
        take' 1 [1,2,3] @?= [1]

    , testCase "take' works on infinite list" $
        take' 4 [1,7 ..] @?= [1,7,13,19]

    , testCase "drop' drops 1 element from 3-element list" $
        drop' 1 [1,2,3] @?= [2,3]

    , testCase "drop' works on empty list" $
        drop' 0 ([] :: [Integer]) @?= []

    , testCase "drop' works on infinite list" $
        take' 1000 (drop' 2 [1..]) @?= take' 1000 [3..]

    , testCase "filter' selects only even numbers from 0 to 10" $
        filter' even [0..10] @?= [0,2..10]

    , testCase "filter' works on empty list" $
        filter' even [] @?= []

    , testCase "filter' works on infinite list" $
        take' 1000 (filter even [1,4..]) @?= take' 1000 [4,10..]

    , testCase "foldl'' can be used for finding sum of elements" $
        foldl'' (+) 0 [1,2,3] @?= 6

    , testCase "foldl'' works on empty list" $
        foldl'' (*) 1 [] @?= 1

    , testCase "concat' works on finite lists as expected" $
        concat' [1,2,3] [4,5,6] @?= [1..6]

    , testCase "concat' works on empty lists" $
        concat' ([] :: [Integer]) ([] :: [Integer]) @?= []

    , testCase "concat' works on infinite 2nd lsist" $
        take' 5 (concat' [3,4,4] [1..]) @?= [3,4,4,1,2]

    , testCase "quickSort' actualy sorts the list" $
        quickSort' [5,2,3,4,1] @?= [1..5]

    , testCase "quickSort' works on empty list" $
        quickSort' ([] :: [Integer]) @?= []
    ]

