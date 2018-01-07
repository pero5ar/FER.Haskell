module Homework where
--
import Data.List
--

-- Task 01

{-
  Inspect type signatures of functions that you are supposed to implement
  and and from that information try to think of how Robot and Bearing 
  data types should look like.
-}

data Robot = Robot {
  robotDirection :: Bearing,
  robotCoordinates :: (Integer, Integer)
} deriving (Eq, Show)
data Bearing = North | East | South | West deriving (Eq, Show)
  
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

bearing :: Robot -> Bearing
bearing = robotDirection

coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

{- It is MANDATORY to implement 'simulate' function in terms of fold -}
simulate :: Robot -> String -> Robot
simulate = foldl action
  where action r 'A' = r { robotCoordinates = advance (robotDirection r) (robotCoordinates r) }
        action r 'L' = r { robotDirection = turnLeft $ robotDirection r }
        action r 'R' = r { robotDirection = turnRight $ robotDirection r }

advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance North (x, y) = (x, y+1)
advance East  (x, y) = (x+1, y)
advance South (x, y) = (x, y-1)
advance West  (x, y) = (x-1, y)

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft East  = North
turnLeft South = East
turnLeft West  = South

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

r = mkRobot North (7, 3)

-- Task 02

data TriangleType

triangleType :: (Ord a, Num a) => a -> a -> a -> TriangleType
triangleType = undefined

-- Task 03

{- some convenient test examples -}
-- splitter " > " " > " => ["", ""]
-- splitter " > " "123 > " => ["123", ""]
-- splitter " > " "123 > 456 > 789" => ["123", "456", "789"]

{-
  you don't have to bother with splitting on an empty list e.g.:
  splitter "" "abcde" => ["a", "b", "c", "d", "e"]
-}

{- It is MANDATORY to implement 'splitter' function in terms of fold -}
splitter :: Eq a => [a] -> [a] -> [[a]]
splitter []    = foldr (\x acc -> if null acc then [[x]] else [x]:acc) []   -- yes I do :)
splitter delim = foldr (\x (acc:accs) -> if delim `isPrefixOf` (x:acc) then []:(drop (length delim - 1) acc):accs else (x:acc):accs) [[]]

-- Task 04

{-
  For this task either write a solution to the problem or if you think
  solution doesn't exist explain why that is the case. Of corse, solution
  must use fold.
-}

{-
  First of all we have to note that "foldl" doesn't work on infinite lists ( http://lambda.jstolarek.com/2012/09/why-foldr-works-for-infinite-lists-and-foldl-doesnt/ ),
  that leaves us with "foldr".

  Currently splitter works like this:
  f x1 ( f x2 (... ( f xn (acc:accs) ) ...) ) 
    where f = if delim `isPrefixOf` (x:acc) then []:(drop (length delim - 1) acc):accs else (x:acc):accs) [[]]
  
  In that implementation we need the accumulator evaluated to produce the next value, 
  otherwise we cannot check the prefix to know how to build the list in the next step.

  This is the core of this problem, to split with fold we need to compare how an element affects the currently accumulated value, 
  i.e. compare a part of it (which part depends on the implementation) to the delimiter.
  I don't see a way this evaluation of the accumulator can be avoided.
-}