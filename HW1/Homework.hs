module Homework where
--
import Data.List
import Data.Char
--

-- Task 01

isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x y = mod x y == 0

isLeapYear :: Int -> Bool
isLeapYear x = isDivisibleBy x 400 || isDivisibleBy x 4 && not (isDivisibleBy x 100)

leapList :: [Int]
leapList = [x | x <- [1996..2017], isLeapYear x]

--

-- Task 02

evaluate :: Double -> [Double] -> Double
evaluate x ys = sum [y * x**k | (k, y) <- zip [0..] ys]

factorial :: Double -> Double  -- this definition is odd because a factorial is only defined for non negative integers
factorial x = product [2..x]

maclaurin :: [Double]
maclaurin = [1/(factorial x) | x <- [0..]]

exp' :: Double -> Double
exp' x = evaluate x $ take 170 maclaurin

--

-- Task 03

items :: (Enum a, Num a) => [(String, a)]
items = [([x], y) | (x, y) <- zip ['a'..'z'] [1..]]

findItem :: [(String, a)] -> String -> [(String, a)]
findItem xs y = [x | x <- xs, fst x == y]

contains :: [(String, a)] -> String -> Bool
contains xs y = or [fst x == y | x <- xs]

lookup :: [(String, a)] -> String -> a
lookup xs y = 
    if contains xs y 
        then snd $ head $ findItem xs y 
        else error "Item does not exist!"

-- a lookup definition without use of findItem or contains:
lookup' :: [(String, a)] -> String -> a
lookup' xs y
    | null xs               = error "Item does not exist!"
    | y == (fst $ head xs)  = snd $ head xs
    | otherwise             = Homework.lookup (tail xs) y

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert xs x = if contains xs (fst x) then xs else x:xs

remove :: [(String, a)] -> String -> [(String, a)]
remove xs y = [x | x <- xs, y /= fst x]

update :: [(String, a)] -> String -> a -> [(String, a)]
update xs y z = 
    if contains xs y 
        then Homework.insert (remove xs y) (y, z)
        else xs

--

-- Task 04

cosineSimilarity :: String -> String -> Double
cosineSimilarity s1 s2 =
    let allWords = getAllWords [s1, s2]
        v1 = constructVector s1 allWords
        v2 = constructVector s2 allWords
    in v1 `dot` v2 / norm v1 / norm v2

-- helpers:

formatWord :: String -> String
formatWord w = [toLower c | c <- w, isLetter c]

formatSentence :: String -> [String]
formatSentence s = [formatWord w | w <- words s]

getAllWords :: [String] -> [String]
getAllWords ss = nub [formatWord w | w <- words $ unwords ss]

count :: [String] -> String -> Int
count ss s = length $ filter (==s) ss

constructVector :: Num a => String -> [String] -> [a]
constructVector s ws = [fromIntegral $ count (formatSentence s) w | w <- ws]

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum [x*y | (x, y) <- zip xs ys]

norm :: Floating a => [a] -> a
norm xs = sqrt $ sum [x*x | x <- xs]

-- data for testing:
_s1 = "Haskell makes me giddy! I want to jump around like a little girl."
_s2 = "Haskell makes me happy! So much that I want to jump around like a little pony."
