module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01
localMaxima :: [Int] -> [Int]
localMaxima (_:_:[]) = []
localMaxima (x1:xs@(x2:x3:_))
    | x1 < x2 && x2 > x3 = x2 : localMaxima xs
    | otherwise          = localMaxima xs

----

-- Task 02
transform :: [(Int, String)] -> [(Char, Int)]
transform []          = []
transform ((i, s):xs) = [(toLower c, i) | c <- s] ++ transform xs 

----

-- Task 03
rule90 :: [Bool] -> [[Bool]]
rule90 xs = xs : rule90 (rule90Step xs)

rule90Step :: [Bool] -> [Bool]
rule90Step xs@(_:x2:_) = (False `xor` x2) : nextElement xs 
    where nextElement [yN1, yN]        = (yN1 `xor` False) : []
          nextElement (y1:ys@(_:y3:_)) = (y1 `xor` y3) : nextElement ys

pretty :: [[Bool]] -> String
pretty []       = "\n"
pretty (xs:xss) = prettyLine xs ++ pretty xss
    where prettyLine []        = "\n"
          prettyLine (True:ys) = '#' : prettyLine ys
          prettyLine (_:ys)    = ' ' : prettyLine ys

-- for testing:
-- putStr $ pretty $ take 8 $ rule90 (replicate 7 False ++ [True] ++ replicate 7 False)

----

-- Task 04
f :: [String]
f = "1" : map (concat . map (\xs@(x0:_) -> (show $ length xs) ++ [x0]) . group) f
