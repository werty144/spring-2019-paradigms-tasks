import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 50
        grusha = robot "Agraphena" 0 100
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack walter @?= 50

        , testCase "Test for getHealth" $
             getHealth walter @?= 50

        , testCase "Test for setName" $
            getName (setName "Volodya" walter) @?= "Volodya"

        , testCase "Test for setAttack" $
            getAttack (setAttack 144 walter) @?= 144

        , testCase "Test for setHealth" $
            getHealth (setHealth 42 walter) @?= 42

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 50"

        , testCase "Test for damage" $
            getHealth (damage grusha 58) @?= 42

        , testCase "Test for isAlive if alive" $
            isAlive grusha @?= True
        , testCase "Test for isAlive if dead" $
            isAlive (damage grusha 100) @?= False

        , testCase "Test for fight" $
            getHealth (fight walter grusha) @?= 50

        , testCase "Test for threeRoundFight" $
            getName (threeRoundFight walter grusha) @?= "Walter"

        , testCase "Test for neueRobotAttack" $
            getHealth (neueRobotAttack grusha) @?= 97

        , testCase "Test for survivors" $
            survivors @?= [robot "R2-D2" 5 2, robot "Galvatron" 1000 1497]
        ]
