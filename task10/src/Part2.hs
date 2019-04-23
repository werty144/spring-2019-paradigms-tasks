module Part2 where

-- Во второй части этого домашнего задания вам предстоит промоделировать битвы роботов
-- Цель этой части показать, как моделировать концепции из объектно-ориентированного
-- программирования в функциональном стиле

-- Про робота можно думать, что это просто тройка из имени, уровня атаки и уровня жизни
-- Разумно думать, что уровни жизни и атаки -- это неотрицательные целые числа

-- это просто псевдонимы для типов(красоты ради)
type Name = String
type Attack = Int
type Health = Int
type Robot = (Name, Attack, Health)

-- Напишем конструктор для робота
robot :: Name -> Attack -> Health -> Robot
robot name attack hp  = (name, attack, hp)

-- У объектов есть геттеры или аксессоры -- функции, которые
-- позволяют нам получить доступ к состоянию объекта
-- Напишем их и мы

getName :: Robot -> Name
getName (myName, _, _) = myName

getAttack :: Robot -> Attack
getAttack (_, myAttack, _) = myAttack

getHealth :: Robot -> Health
getHealth (_, _, myHealth) = myHealth

-- Задание 1
-- Аналогичным образом напишите сеттеры, функции, которые устанавливают
-- состояние робота

setName :: Name -> Robot -> Robot
setName = undefined

setAttack :: Attack -> Robot -> Robot
setAttack = undefined

setHealth :: Health -> Robot -> Robot
setHealth = undefined

-- Задание 2.
-- Напишите функцию, которая ведет себя как __str__
-- То есть возвращает строковое представление о роботе в виде:
-- > marvin = robot "Marvin" 100 500
-- > printRobot marvin
-- > "Marvin, attack: 100, health: 500"

printRobot :: Robot -> String
printRobot = undefined

-- Давайте теперь научим роботов драться друг с другом
-- Напишем функцию damage которая причиняет роботу урон
damage :: Robot -> Int -> Robot
damage victim amount = let
        name = getName victim
        attack = getAttack victim
        health = getHealth victim
        newHealth = health - amount
    in robot name attack newHealth

-- Задание 3.
-- Используя функцию damage, напишите функцию, которая моделирует один раунд схватки между
-- двумя роботами, возвращая робота, который был атакован с новым состоянием
fight :: Robot -> Robot -> Robot
fight attacker defender = undefined

-- Затем напишите функцию, которая бы моделировала три раунда схватки между
-- двумя роботами и возвращала бы победителя. Победителем считайте того робота,
-- у которого уровень жизни больше либо равен уровня жизни другого робота
threeRoundFight :: Robot -> Robot -> Robot
threeRoundFight attacker defender = undefined

-- Задание 4.
-- Создайте список из трех роботов
roboter :: [Robot]
roboter = undefined

-- Затем создайте четвертого
neueRobot :: Robot
neueRobot = undefined

-- Напишите замыкание для метода fight, которое бы позволяло четвертому роботу
-- сразиться с первыми тремя используя функцию map
neueRobotAttak :: Robot -> Robot
neueRobotAttak = undefined

-- Наконец, используя filter определите, кто из роботов выжил,
-- То есть имел положительный уровень жизни
survivors :: [Robot]
survivors = undefined
