{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--


{-LECTURE 04-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-04.lhs

-- EXERCISE 01 =======================================================================

-- Define 'headHunter xss' that takes the head of the first list element. If 
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third element.
-- If none of this works, the function returns an error.
ex411 = headHunter
headHunter :: [[a]] -> a
headHunter ((x:_):_)        = x
headHunter ([]:(x:_):_)     = x
headHunter ([]:[]:(x:_):_)  = x
headHunter _                = error "first three lists are empty"

-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.
ex412 = firstColumn
firstColumn :: [[a]] -> [a]
firstColumn m@(hr:_) =
    if and [length hr == length row | row <- m]
        then [h | row@(h:_) <- m]
        else error "not a valid matrix!"

-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"
ex413 = shoutOutLoud
shoutOutLoud :: String -> String
shoutOutLoud s = unwords [ c:c:c:p | (c:p) <- words s ]

-- EXERCISE 02 =======================================================================

-- Define 'pad' that pads the shorter of two the strings with trailing spaces 
-- and returns both strings capitalized.
-- pad :: String -> String -> (String, String)
-- pad "elephant" "cat" => ("Elephant", "Cat     ")
ex421 = pad
pad :: String -> String -> (String, String)
pad (x:xs) (y:ys) = (toUpper x : (padTo xs $ length ys), toUpper y : (padTo ys $ length xs))
    where padTo s n = s ++ replicate (n - length s) ' '

-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a list
-- sorted in ascending order. (You can use the built-int 'splitAt' function and
-- the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)
ex422 = quartiles
quartiles :: [Int] -> (Double, Double, Double)
quartiles l = (median q1Area, q2, median q3Area)
    where sorted = sort l
          q2 = median sorted
          splited = splitAt (ceiling q2) sorted
          q1Area = fst splited
          q3Area = tail $ snd splited
          
-- "the previously defined 'median' function." :
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

-- EXERCISE 03 =======================================================================

-- Redo Exercise 2 using 'let' instead of 'where'.
ex431 = pad'
pad' :: String -> String -> (String, String)
pad' (x:xs) (y:ys) = 
    let padTo s n = s ++ replicate (n - length s) ' '
    in (toUpper x : (padTo xs $ length ys), toUpper y : (padTo ys $ length xs))

ex432 = quartiles'
quartiles' :: [Int] -> (Double, Double, Double)
quartiles' l =
    let sorted = sort l
        q2 = median sorted
        splited = splitAt (ceiling q2) sorted
        q1Area = fst splited
        q3Area = tail $ snd splited
    in (median q1Area, q2, median q3Area)

-- EXERCISE 04 =======================================================================

-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"
ex441 = foo
foo :: Show c => (Int, Int) -> [c] -> String
foo x (_:y:_) =
    "The pair " ++ 
        ( case x of 
            (1, 1) -> "contains two onea"
            (_, 1) -> "contains one one"
            (1, _) -> "contains one one"
            (_, _) -> "does not contain a single one"
        ) 
    ++ " and the second element of the list is " ++ show y

{-LECTURE 05-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-05.lhs

-- EXERCISE 01 =======================================================================

-- Define a recursive function to compute the product of a list of elements.
-- product' :: Num a => [a] -> a
ex511 = product'
product' :: Num a => [a] -> a
product' []     = error "NaN"
product' [x]    = x
product' (x:xs) = x * product' xs

-- Define a recursive function 'headsOf' that takes a list of lists and
-- returns a list of their heads.
-- headsOf :: [[a]] -> [a]
-- headsOf [[1,2,3],[4,5],[6]] => [1,4,6]
ex512 = headsOf
headsOf :: [[a]] -> [a]
headsOf []       = []
headsOf ([]:xss) = headsOf xss
headsOf (xs:xss) = head xs : headsOf xss

-- EXERCISE 02 =======================================================================

-- Define a recursive function 'modMult n m xs' that multiplies each element of
-- a list 'xs' with 'n' modulo 'm'.
ex521 = modMult
modMult :: (Integral a) => a -> a -> [a] -> [a]
modMult n m xs = mult xs
    where f = n `mod` m
          mult [] = []
          mult (x:xs) = f * x : mult xs

-- Define a function 'addPredecessor' that adds to each element of a list the
-- value of the preceding element. The first element gets no value added.
-- addPredecessor :: Num a => [a] -> [a]
-- addPredecessor [3,2,1] => [3,5,3]
ex522 = addPredecessor
addPredecessor :: Num a => [a] -> [a]
addPredecessor xs = add 0 xs
    where add _ []     = []
          add n (x:xs) = x + n : add x xs

-- EXERCISE 03 =======================================================================

-- Define 'equalTriplets' that filters from a list of triplets (x,y,z) all
-- triplets for which x==y==z.
-- equalTriplets [(1,2,3),(2,2,2),(4,5,6)] => [(2,2,2)]
ex531 = equalTriplets
equalTriplets :: Eq a => [(a, a, a)] -> [(a, a, a)]
equalTriplets [] = []
equalTriplets ((x1,x2,x3):xs)
    | x1 == x2 && x2 == x3  = (x1,x2,x3) : equalTriplets xs
    | otherwise             = equalTriplets xs

-- Define your own version of the replicate function:
-- replicate' :: Int -> a -> [a]
ex532 = replicate'
replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x

-- EXERCISE 04 =======================================================================

-- Define your own recursive version of the drop function:
-- drop' :: Int -> [a] -> [a].
-- Define drop'' (a wrapper function) so that for n < 0 the function drops
-- the elements from the end of the list. You can use 'reverse'.
ex541 = drop'
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
    | n <= 0    = x : drop' 0 xs
    | otherwise = drop' (n-1) xs

ex541' = drop''
drop'' :: Int -> [a] -> [a]
drop'' n xs
    | n < 0     = drop' (-n) $ reverse xs
    | otherwise = drop' n xs

-- Define a recursive function 'takeFromTo n1 n2 xs'.
-- takeFromTo :: Int -> Int -> [a] -> [a]
ex542 = takeFromTo
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ [] = []
takeFromTo n1 n2 (x:xs)
    | n1 <= 0 && n2 > 0 = x : takeFromTo 0 (n2-1) xs
    | otherwise         = takeFromTo (n1-1) (n2-1) xs

-- EXERCISE 05 =======================================================================

-- Define a recursive function 'eachThird' that retains every third element
-- in a list.
-- eachThird :: [a] -> [a]
-- eachThird "zagreb" => "gb"
ex551 = eachThird
eachThird :: [a] -> [a]
eachThird xs = each 2 xs
    where f = 2
          each _ [] = []
          each 0 (x:xs) = x : each f xs
          each n (x:xs) = each (n-1) xs

-- Define a recursive function 'crossZip' that zips two lists in a "crossing"
-- manner:
-- crossZip [1,2,3,4,5] [4,5,6,7,8] => [(1,5),(2,4),(3,7),(4,6)]
ex552 = crossZip
crossZip :: [a] -> [b] -> [(a,b)]
crossZip []         _          = []
crossZip _          []         = []
crossZip [x]        [y]        = []
crossZip [x]        (y1:y2:ys) = (x, y2) : crossZip [] ys
crossZip (x1:x2:xs) [y]        = (x2, y) : crossZip xs []
crossZip (x1:x2:xs) (y1:y2:ys) = (x1, y2) : (x2, y1) : crossZip xs ys

-- EXERCISE 06 =======================================================================

-- Write an accumulator-style recursive definition of
-- length' :: [a] -> Int
ex561 = length'
length' :: [a] -> Int
length' xs = len xs 0
    where len []     n = n
          len (x:xs) n = let acc = n + 1 in acc `seq` len xs acc

-- Write an accumulator-style recursive definition of
--     maxUnzip :: [(Int, Int)] -> (Int, Int)
-- that returns the maximum element at the first position and the maximum
-- element at the second position in a pair, i.e., it's equivalent to:
--     maxUnzip zs = (maximum xs, maximum ys)
--         where (xs,ys) = unzip zs
-- If the list is empty, return an "empty list" error.
-- Now write a standard recursive definition maxUnzip' (without an accumulator).
ex562 = maxUnzip
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip ((x, y):zs) = innerMaxUnzip x y zs
    where innerMaxUnzip maxX maxY []          = (maxX, maxY)
          innerMaxUnzip maxX maxY ((x, y):zs) = innerMaxUnzip (max maxX x) (max maxY y) zs

ex562' = maxUnzip'
maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' [(x, y)]    = (x, y)
maxUnzip' ((x, y):zs) = (max x xs, max y ys)
    where (xs, ys) = maxUnzip' zs
