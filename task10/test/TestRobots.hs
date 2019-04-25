module TestRobots where

import Test.Tasty
import Test.Tasty.HUnit

import Robots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
    in testGroup "Unit tests for Robots task"
        [ testCase "Meet Walter!" $
            getName walter @?= "Walter"
        , testCase "Walter is awesome" $
            printRobot walter @?= "Walter, attack: 50, health: 50"
        ]
