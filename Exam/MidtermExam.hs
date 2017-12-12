module MidtermExam where
--
import Data.List
import Data.Char (toLower)
--

{-
    Problem 01 - (3 points)

    Write a function `listOfDigits :: [Int] -> [Int]` which takes a list of
    nonnegative integers and produces a list of their consecutive digits.
-}

listOfDigits :: [Int] -> [Int]
listOfDigits xs = concat $ map (\x -> reverse $ digits x) xs
    where digits x = x `mod` 10 : if x > 9 then digits (x `div` 10) else []
        

{-
    Tests for Problem 01
-}

p1t1 :: Bool
p1t1 = listOfDigits [123,375,0,42] == [1,2,3,3,7,5,0,4,2]

p1t2 :: Bool
p1t2 = take 20 (listOfDigits [1..]) == [1,2,3,4,5,6,7,8,9,1,0,1,1,1,2,1,3,1,4,1]

--

{-
    Problem 02 - (4 points)

    Implement the function `anagramsFor :: String -> [String] -> [String]`
    that, given a word and a list of possible anagrams, select the correct
    sublist.

    Given "listen" and a list of candidates like "enlists" "google" "inLets"
    "banana" the program should return a list containing "inLets".

    NOTE: The input is NOT case-sensitive. "INLetS" and "inLeTs" should be
    the same.
-}

anagramsFor :: String -> [String] -> [String]
anagramsFor t xs =  let strToLower = map toLower
                        anagrams = permutations $ strToLower t
                    in filter (\x -> strToLower x `elem` anagrams) xs

{-
    Tests for Problem 02
-}

p2t1 :: Bool
p2t1 = anagramsFor "listen" ["enlists", "google", "inlets", "banana", "letins"] == ["inlets", "letins"]

p2t2 :: Bool
p2t2 = anagramsFor "listen" ["enlists", "google", "inLets", "banana"] == ["inLets"]

--

{-
    Problem 03 - (3 points)

    Define a triangle data type which stores length of its 3 sides as `Int`.
    To make sure proper triangle is created provide a "constructor" function
    `makeTriangle :: Int -> Int -> Int -> Maybe Triangle` which checks if
    triangle is valid and than returns just a new triangle or nothing
    otherwise.

    Triangle is valid if any side is less than the sum of the other two sides.

    Here is a definition of `Maybe` data type (which is already included in
    `Prelude`).

    data Maybe a = Just a | Nothing
-}

data Triangle = Triangle Int Int Int

makeTriangle :: Int -> Int -> Int -> Maybe Triangle
makeTriangle x y z = if (x + y > z) && (x + z > y) && (z + y > x) then Just $ Triangle x y z else Nothing

{-
    Tests for Problem 03
-}

p3t1 :: Bool
p3t1 = case makeTriangle 100 20 20 of { Nothing -> True; Just _ -> False; }

p3t2 :: Bool
p3t2 = case makeTriangle 3 4 5 of { Nothing -> False; Just _ -> True; }