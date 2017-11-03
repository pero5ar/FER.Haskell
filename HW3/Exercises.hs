{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--


{-LECTURE 06-} -- http://www.fer.unizg.hr/_download/repository/puh-2017-lecture-06.lhs

-- EXERCISE 01 =======================================================================

{-
  1.1.
  - Write an accumulator-style recursive definition of
    length' :: [a] -> Int
-}

ex611 = length'
length' :: [a] -> Int
length' xs = len xs 0
  where len []     n = n
        len (x:xs) n = let acc = n + 1 in acc `seq` len xs acc

{-
  1.2
  - Write an accumulator-style recursive definition of
      maxUnzip :: [(Int, Int)] -> (Int, Int)
    that returns the maximum element at the first position and the maximum
    element at the second position in a pair, i.e., it's equivalent to:
      maxUnzip zs = (maximum xs, maximum ys)
        where (xs,ys) = unzip zs
    If the list is empty, return an "empty list" error.

  - Now write a standard recursive definition (without an accumulator).
-}
ex612 = maxUnzip
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip ((x, y):zs) = innerMaxUnzip x y zs
  where innerMaxUnzip maxX maxY []          = (maxX, maxY)
        innerMaxUnzip maxX maxY ((x, y):zs) = innerMaxUnzip (max maxX x) (max maxY y) zs

ex662' = maxUnzip'
maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' [(x, y)]    = (x, y)
maxUnzip' ((x, y):zs) = (max x xs, max y ys)
  where (xs, ys) = maxUnzip' zs
