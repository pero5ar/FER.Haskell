module Homework where
--
import Data.List
import Data.Maybe
import Data.Time.Clock ( UTCTime(..) )
import Data.Time.Calendar ( Day, gregorianMonthLength, fromGregorian )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Data.Char (isSpace)
import Control.Monad (forM_)
import System.IO
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

{-- 
  Don't get scared of the code ahead, I misinterpreted the task in the begining 
  and though I needed some rose tree structure for the hiearchy.
  At the point of realizing that parseCategories retuns a list, I already had 
  a lot of code already done and working, so I decided to just go with it and 
  finish it with that logic.
--}

-- Helpers:
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

trim :: String -> String
trim = dropAndReverse . dropAndReverse
  where dropAndReverse = reverse . dropWhile isSpace 

splitAtFirst :: (Eq a) => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (== x)
--

-- Category related code:
data Category = NullCategory | Category String [Category] deriving Show

isNullCategory :: Category -> Bool
isNullCategory NullCategory = True
isNullCategory _            = False

getName :: Category -> String
getName NullCategory      = ""
getName (Category name _) = name

(~==) :: Category -> Category -> Bool   -- similarity operator
(~==) c1 c2 = getName c1 == getName c2

similarElem :: Category -> [Category] -> Bool
similarElem c = not . isNothing . find (~== c)

getSimilar :: Category -> [Category] -> Category
getSimilar c = fromJust . find (~== c)

mergeCategory :: Category -> Category -> Category
mergeCategory c1 NullCategory = c1
mergeCategory NullCategory c2 = c2
mergeCategory (Category name1 children1) (Category name2 children2)
  | name1 /= name2 = error "Categories with different names cannot be merged"
  | otherwise      = 
    let duplicates1 = filter (`similarElem` children2) children1
        unique1 = filter (not . (`similarElem` duplicates1)) children1
        duplicates2 = filter (`similarElem` children1) children2
        unique2 = filter (not . (`similarElem` duplicates2)) children2
        mergedCategories = map (\c -> mergeCategory c $ getSimilar c duplicates2) duplicates1
    in Category name1 (unique1 ++ unique2 ++ mergedCategories)

addCategory :: Category -> Category -> Category
addCategory root NullCategory = root
addCategory (Category rootName children) elem =
  let category = find (~== elem) children
  in if isNothing category
    then Category rootName (elem:children)
    else let mergedElem = mergeCategory elem $ fromJust category
             otherChildren = filter (not . (~== elem)) children
         in Category rootName (mergedElem:otherChildren)

categoryTreeToList :: Category -> [Category] -- builds a list for every tree path (root to leaf)
categoryTreeToList NullCategory = [NullCategory]
categoryTreeToList (Category name children)
  = let listify = if name == rootCategoryName then categoryTreeToList else map (\l -> Category name [l]) . categoryTreeToList 
    in foldr (\child acc -> acc ++ listify child) [] children

rootCategoryName :: String
rootCategoryName = ">"

parseCategory :: String -> Category
parseCategory "" = NullCategory
parseCategory s  = let (name, child) = mapTuple trim $ splitAtFirst '>' s
                       childCategory = parseCategory child
                   in Category name $ if isNullCategory childCategory then [NullCategory] else [childCategory, NullCategory]

parseCategories :: [String] -> [Category]
parseCategories = categoryTreeToList . foldr (\s root -> addCategory root $ parseCategory s) root
    where root = Category rootCategoryName []

printCategory :: Category -> [String]
printCategory NullCategory = [""]
printCategory (Category name children) 
  = foldr (\child acc -> let str = if name == rootCategoryName then ""
                                    else if isNullCategory child then name else name ++ " > "
                         in (++) acc $ map (str ++) $ printCategory child)
    [] children

printCategories :: [Category] -> [String]
printCategories = concatMap printCategory
--

-- From file (e.g. "HWTask4.txt")
main :: FilePath -> IO ()
main f = do 
  h <- openFile f ReadMode
  s <- hGetContents h
  let categories = printCategories . parseCategories $ lines s
  forM_ categories putStrLn
  hClose h
--

-- For Testing:

test :: [String]
test = printCategories $ parseCategories testData

test' :: [String]
test' = printCategories $ parseCategories $ drop 3 testData

testMerge :: Category
testMerge = mergeCategory (parseCategory $ testData!!3) $ mergeCategory (parseCategory $ testData!!0) (parseCategory $ testData!!1)

testAdd :: Category
testAdd = foldr (\s root -> addCategory root $ parseCategory s) root $ drop 3 testData
  where root = Category rootCategoryName []

testTreeToList :: [Category]
testTreeToList = categoryTreeToList . foldr (\s root -> addCategory root $ parseCategory s) root $ drop 3 testData
  where root = Category rootCategoryName []

testData :: [String]
testData = [
  "Computer > Equipment > Mouse pads",
  "Computer > Equipment > Keyboards",
  "Computer > Equipment",
  "Computer > Laptops",
  "Computer > Laptops > 15 inch",
  "Computer > Components > SSD",
  "Food > Fruit > Orange"
  ]