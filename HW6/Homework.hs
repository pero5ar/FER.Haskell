module Homework where
--
import Data.List
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.Calendar ( Day, gregorianMonthLength, fromGregorian )
import Data.Time.Format ( formatTime, defaultTimeLocale )
--

-- Task 01
data Pred 
  = Val Bool 
  | Not Pred
  | And Pred Pred
  | Or Pred Pred


eval :: Pred -> Bool
eval (Val value)     = value
eval (Not exp)       = not $ eval exp
eval (And exp1 exp2) = eval exp1 && eval exp2
eval (Or  exp1 exp2) = eval exp1 || eval exp2

-- Task 02

dateFromDescription :: String -> Day
dateFromDescription = undefined

-- Task 03

data Tree a
  = Leaf | Node a (Tree a) (Tree a)
  deriving (Eq, Show)

testTree :: Tree Int
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- a)
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _  Leaf            = Leaf
treeFilter f (Node x lxs rxs) = if f x then Node x (treeFilter f lxs) (treeFilter f rxs) else Leaf 

-- b)
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f tree = mapWithLevel 0 f tree
  where mapWithLevel _ _ Leaf = Leaf
        mapWithLevel d f (Node x lxs rxs) 
          = let mapNext = mapWithLevel (d+1) f in Node (f d x) (mapNext lxs) (mapNext rxs)

-- c)
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf  Leaf                             = True
isSubtree _     Leaf                             = False
isSubtree Leaf  (Node y lys rys)                 = isSubtree Leaf lys || isSubtree Leaf rys
isSubtree treeX treeY@(Node y lys rys)           = or [ isSubtreeFixed treeX treeY, isSubtree treeX lys, isSubtree treeX rys ]
  where isSubtreeFixed Leaf             Leaf             = True
        isSubtreeFixed Leaf             _                = False
        isSubtreeFixed _                Leaf             = False
        isSubtreeFixed (Node x lxs rxs) (Node y lys rys) = and [ x == y, isSubtreeFixed lxs lys, isSubtreeFixed rxs rys ]

-- Task 04
data Category

parseCategories :: [String] -> [Category]
parseCategories = undefined

printCategories :: [Category] -> [String]
printCategories = undefined