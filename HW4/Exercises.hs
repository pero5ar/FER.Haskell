{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--


{-LECTURE 07-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-07.lhs

-- EXERCISE 01 =======================================================================
{-
  Define the following functions using partial application of existing functions:
-}

{-
  1.1.
  - Function 'takeThree' that takes the first three elements from a list.
  - Function 'dropThree' that drops the first three elements from a list.
  - Function 'hundredTimes' that takes one element and repeats it 100 times in a
    list.
-}

ex711' = takeThree
takeThree :: [a] -> [a]
takeThree = take 3

ex711'' = dropThree
dropThree :: [a] -> [a]
dropThree = drop 3

ex711''' = hundredTimes
hundredTimes :: a -> [a]
hundredTimes = replicate 100

{-
  1.2.
  - Define 'index' that indexes the elements in a list:
      index "xyz" => [(0,'x'),(1,'y'),(2,'z')]
  - Define index' in which the index comes at the second position in the pair.
-}

ex712 = index
index :: [a] -> [(Integer, a)]
index = zip [0..]

ex712' = index'
index' :: [a] -> [(a, Integer)]
index' = (`zip` [0..])

{-
  1.3.
  - Define 'divider n' that returns a string of length 'n' consisting of
    characters '='.
    divider :: Int -> [Char]
    divider 3 => "==="
-}

ex713 = divider
divider :: Int -> [Char]
divider = (`replicate` '=')

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define 'applyOnLast f xs ys' that applies a binary function 'f' on the last 
    element of 'xs' and the last element of 'ys'.
    applyOnLast (+) [1,2,3] [5,6] => 9
    applyOnLast max [1,2] [3,4] => 4
  - Using this function and the 'addThree' function, define
    lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
    lastTwoPlus100 [1,2,3] [6,5] => 108
-}
addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

ex721 = applyOnLast
applyOnLast :: (a -> b -> c) -> [a] -> [b] -> c
applyOnLast f xs ys = f (last xs) (last ys)

ex721'' = lastTwoPlus100
lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 = applyOnLast $ addThree 100

{-
  2.2.
  - Define 'applyManyTimes n f x' that applies 'n' times function 'f' to argument
    'x'. If n<=0, return 'x' unaltered.
    applyManyTimes 5 (+2) 0 => 10
    applyManyTimes 3 finishSentence "hm" => "hm..."
  - Using this function, define 'applyTwice''
-}

ex722 = applyManyTimes
applyManyTimes :: Integer -> (a -> a) -> a -> a
applyManyTimes n f x
    | n <= 0    = x
    | otherwise = let f' = f x in f' `seq` applyManyTimes (n-1) f f'

ex722' = applyTwice'
applyTwice' :: (a -> a) -> a -> a
applyTwice' = applyManyTimes 2

-- EXERCISE 03 =======================================================================
{-
  Write the following functions using 'map'.
-}

{-
  3.1.
  - listifylist :: [a] -> [[a]]
    listifylist [1,2,3] => [[1],[2],[3]]
-}

ex731 = listifylist
listifylist :: [a] -> [[a]]
listifylist = map (:[])

{-
  3.2.
  - Define 'cutoff n xs', which cuts off all values from the lists 'xs' at 
    value 'n'.
    cutoff :: Int -> [Int] -> [Int]
    cutoff 100 [20,202,34,117] => [20,100,34,100]
-}

ex732 = cutoff
cutoff :: Int -> [Int] -> [Int]
cutoff n = map (min n)

-- EXERCISE 04 =======================================================================
{-
  Define the following functions using 'map' and 'filter':
-}

{-
  4.1.
  - Function 'sumEvenSquares' that adds the squares of all even numbers from a
    list.
    sumEvenSquares :: [Integer] -> Integer
    sumEvenSquares [1,2,3,4] => 20
-}
ex741 = sumEvenSquares
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares = sum . map (^2) . filter even

{-
  4.2.
  - Function 'freq x xs' that counts how many times element 'x' occurs in list
    'xs'.
    freq :: Eq a => a -> [a] -> Int
    freq 'k' "kikiriki" => 3
-}
ex742 = freq
freq :: Eq a => a -> [a] -> Int
freq x = length . filter (==x)

{-
  4.3.
  - Function 'freqFilter n' that filters all elements that occur at least 'n'
    times in a list.
    freqFilter :: Eq a => Int -> [a] -> [a]
    freqFilter 4 "kikiriki" => "iiii"
-}
ex743 = freqFilter
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\x -> n <= freq x xs) xs

-- EXERCISE 05 =======================================================================
{-
  Define the following functions using lambda expressions:
-}

{-
  5.1.
  - Define a function 'withinInterval n m xs' that filters from list 'xs' all
    elements that fall within the [n,m] interval.
-}
ex751 = withinInterval
withinInterval :: Ord a => a -> a -> [a] -> [a]
withinInterval n m = filter (\x -> n <= x && x <= m)

{-
  5.2.
  - Define 'sndColumn m' that returns the second column of matrix 'm',
    represented as a list of lists.
    sndColumn [[a]] -> [a]
    sndColumn [[1,2,3],[4,5,6]] => [2,5]
-}
ex752 = sndColumn
sndColumn :: [[a]] -> [a]
sndColumn = map (!! 1)

{-
  5.3.
  - Define 'canoinicalizePairs' that takes a list of pairs and returns a list of
    pairs with the order of elements switched so that the first element of the
    pair is smaller than the second one. If the elements are equal, the pair is
    discarded. 
    canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
    canonicalizePairs [(4,1),(2,2),(1,5)] => [(1,4),(1,5)]
-}
ex753 = canonicalizePairs
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs = map (\(x, y) -> (min x y, max x y)) . filter (\(x, y) -> x /= y) 