{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.Char (toUpper, isUpper)
import Data.List (sortBy, findIndices, intercalate)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)


{-LECTURE 08-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-08.lhs

-- EXERCISE 01 =======================================================================
{-
  Define the following functions using composition and pointfree style (you may
  of course use local definitions):
-}

{-
  1.1.
  - Define 'sumEven' that adds up elements occurring at even (incl. zero) 
    positions in a list.
    sumEven :: [Integer] -> Integer
    sumEven [1..10] => 25
-}

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]

{-
  1.2.
  - Define 'filterWords ws s' that removes from string 's' all words contained
    in the list 'ws'.
    filterWords :: [String] -> String -> String
-}

filterWords :: [String] -> String -> String
filterWords ws s = unwords . filter (`notElem` ws) $ words s

{-
  1.3.
  - Define 'initials3 d p s' that takes a string 's' and turns it into a string
    of initials. The function is similar to 'initials2' but additionally delimits
    the initials with string 'd' and discards the initials of words that don't
    satisfy the predicate 'p'.
    initials3 :: String -> (String -> Bool) -> String -> String
    initials3 "." (/="that") "a company that makes everything" => "A.C.M.E."
  - Use this function to define the 'initials' function.
-}

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p s = concat . map ((:d) . toUpper . head) . filter p $ words s

-- EXERCISE 02 =======================================================================
{-
  Just a reminder that EVERY function in this file needs to have a type signature ;)
-}

{-
  2.1.
  - Define 'maxDiff xs' that returns the maximum difference between consecutive
    elements in the list 'xs'.
    maxDiff :: [Int] -> Int
    maxDiff [1,2,3,5,1] => 4
  - Define 'maxMinDiff' that returns the pair (min_difference, max_difference).
-}

maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (abs . uncurry (-)) . zip xs $ tail xs

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (maximum diffs, minimum diffs)
  where diffs = map (abs . uncurry (-)) . zip xs $ tail xs

{-
  2.2.
  - Define 'studentsPassed' that takes as input a list [(NameSurname,Score)] and
    returns the names of all students who scored at least 50% of the maximum 
    score.
-}
type NameSurname = (String, String)
-- type Score a = (Ord a, Fractional a) -- this needs something called "ConstraintKind" enabled in GHC :/

studentsPassed :: (Ord a, Fractional a) => [(NameSurname, a)] -> [String]
studentsPassed xs = map (fst . fst) . filter (\(_, score) -> score >= passingScore) $ xs
  where passingScore = (*) 0.5 $ maximum . map snd $ xs
-- testing data:
-- studenti = [(("Pero", "Peric"), 22.5), (("Ivan", "Horvat"), 8), (("Pajo", "Patak"), 15)]

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define 'isTitleCased' that checks whether every word in a string is
    capitalized.
    isTitleCased :: String -> Bool
    isTitleCased "University Of Zagreb" => True
-}

isTitleCased :: String -> Bool
isTitleCased s = all (isUpper . head) $ words s

{-
  3.2.
  - Define 'sortPairs' that sorts the list of pairs in ascending order with
    respect to the second element of a pair.
-}

sortPairs :: Ord b => [(a, b)] -> [(a, b)]
sortPairs = sortBy (comparing snd)

{-
  3.3.
  - Define 'filename' that extracts the the name of the file from a file path.
    filename :: String -> String
    filename "/etc/init/cron.conf" => "cron.conf"
-}

filename :: String -> String
filename = Prelude.reverse . takeWhile (/= '/') . Prelude.reverse

{-
  3.4.
  - Define 'maxElemIndices' that returns the indices of the maximum element in a
    list. Return "empty list" error if the list is empty.
    maxElemIndices :: Ord a => [a] -> [Int]
    maxElemIndices [1,3,4,1,3,4] => [2,5]
-}

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = error "empty list"
maxElemIndices xs = findIndices (== maximum xs) xs

-- EXERCISE 04 =======================================================================

{-
  4.1. 
  - Define 'elem'' using 'foldr'.
-}

elem :: Eq a => a -> [a] -> Bool
elem y = foldr (\x acc -> acc || y == x) False

{-
  4.2.
  - Define 'reverse' using 'foldr'.
-}

reverse :: [a] -> [a]
reverse = foldr ((\x acc -> acc ++ [x])) []
  
{-
  4.3.
  - Using 'foldr' define 'nubRuns' that removes consecutively repeated elements
    from a list.
    nubRuns :: Eq a => [a] -> [a]
    nubRuns "Mississippi" => "Misisipi"
-}

nubRuns :: Eq a => [a] -> [a]
nubRuns xs = foldr (\x acc -> if x == head acc then acc else x:acc) [last xs] xs

-- EXERCISE 05 =======================================================================

{-
  5.1.
  - Write 'reverse' using 'foldl'.
    reverse' :: [a] -> [a]
-}

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

{-
  5.2.
  - Using 'foldl' define function 'sumEven' from problem 1.1.
-}

sumEven' :: [Integer] -> Integer
sumEven' = foldl (\acc (i, x) -> if even i then x + acc else acc) 0 . zip [0..]

{-
  5.3.
  - Using 'foldl' define maxUnzip :: [(Int,Int)] -> (Int,Int) 
    that returns the maximum element at first position in a pair and maximum
    element at second position in the pair. In other words, the function should
    be equivalent to:
      maxUnzip zs = (maximum xs, maximum ys)
        where (xs,ys) = unzip zs
    Return "empty list" error if the list is empty.
-}

maxUnzip :: [(Int,Int)] -> (Int,Int) 
maxUnzip [] = error "empty list"
maxUnzip xs = foldl1 (\(maxX, maxY) (x, y) -> (max maxX x, max maxY y)) xs


{-LECTURE 09-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-09.lhs

-- EXERCISE 01 =======================================================================

{-
  1.1.
  - Define a 'Date' structure with the appropriate fields.
  - Define a function that shows a date in the DD.MM.YYYY format (without
    leading zeroes).
    showDate :: Date -> String
-}
data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date d m y) = intercalate "." $ map show [d, m, y]

{-
  1.2.
  - Define a function
    translate :: Point -> Shape2 -> Shape2
    that translates a shape into the direction of vector (x,y).
-}

data Point = Point Double Double 
  deriving Show

data Shape2 = Circle2 Point Double | Rectangle2 Point Point 
  deriving Show

translate :: Point -> Shape2 -> Shape2  
translate (Point x y) (Circle2 (Point x0 y0) r) = Circle2 (Point (x0+x) (y0+y)) r
translate (Point x y) (Rectangle2 (Point x0 y0) (Point x1 y1)) = Rectangle2 (Point (x0+x) (y0+y)) (Point (x1+x) (y1+y))

{-
  1.3.
  - Write a function 'inShape' that tests whether a point is contained within a
    given shape (or is on its border).
    inShape :: Shape2 -> Point -> Bool
  - Write a function 'inShapes' that tests if the point is within any shape from
    the list of shapes.
    inShapes :: [Shape2] -> Point -> Bool
-}

inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point x0 y0) r) (Point x y) = r * r >= (x0-x) * (x0-x) + (y0-y) * (y0-y)
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) = 
  let (top, bottom) = (max x1 x2, min x1 x2)
      (left, right) = (min y1 y2, max y1 y2)
  in top >= x && x >= bottom && left <= y && y <= right

inShapes :: [Shape2] -> Point -> Bool
inShapes xs p = any (`inShape` p) xs

{-
  1.4.
  - Define your type 'Vehicle' that can be a 'Car', 'Truck', 
    'Motorcycle', or 'Bicycle'. The first three store a name of the manufacturer
    (String) and horsepower (Double).
  - Write a function 'totalHorsepower' that adds up the horsepower of the
    vehicles, assuming that bicycle's horsepower is 0.2.
-}

type Manufacturer = String
type Horsepower = Double

data Vehicle = 
    Car Manufacturer Horsepower
  | Truck Manufacturer Horsepower
  | Motorcycle Manufacturer Horsepower
  | Bicycle

totalHorsepower :: [Vehicle] -> Double
totalHorsepower xs = sum $ map hp xs
  where hp (Bicycle)        = 0.2
        hp (Car _ h)        = h
        hp (Truck _ h)      = h
        hp (Motorcycle _ h) = h

-- testing data:
-- vehicles = [Bicycle, (Car "Fiat" 66.6), (Truck "MAN" 786.3), (Motorcycle "Tomos" 34.5), Bicycle]

-- EXERCISE 02 =======================================================================

data Level = Bachelor | Master | PhD deriving (Show, Eq)

data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double
  } deriving Show

-- testing data:
{-
s1 = Student { studentId = "0036491211"
  , firstName = "John", lastName = "Doe"
  , level = Master, avgGrade = 2} 
s2 = Student { studentId = "0036491212"
  , firstName = "A", lastName = "Doe"
  , level = Bachelor, avgGrade = 5} 
s3 = Student { studentId = "0036491213"
  , firstName = "S", lastName = "Doe"
  , level = Bachelor, avgGrade = 3.1 } 
s4 = Student { studentId = "0036491214"
  , firstName = "D", lastName = "Doe"
  , level = Bachelor, avgGrade = 4.1 } 
s5 = Student { studentId = "0036491215"
  , firstName = "F", lastName = "Doe"
  , level = PhD, avgGrade = 2.5 } 
s6 = Student { studentId = "0036491215"
  , firstName = "G", lastName = "Doe"
  , level = PhD, avgGrade = 3 } 
ss = [s1, s2, s3, s4, s5]
-}

{-
  2.1.
  - Define a function that increases the average grade of the student by 1.0,
    but not above 5.0.
    improveStudent :: Student -> Student
-}

improveStudent :: Student -> Student
improveStudent s = s { avgGrade = min 5 $ avgGrade s + 1 }

{-
  2.2.
  - Write a function to compute the average grade of students for the different
    study levels.
    avgGradePerLevels :: [Student] -> (Double, Double, Double)
-}

avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels students = 
  let bacc = filter ((==Bachelor) . level) students
      mag = filter ((==Master) . level) students
      ph = filter ((==PhD) . level) students
  in (avg bacc, avg mag, avg ph)
  where avg students = (sum $ map avgGrade students) / (fromIntegral $ length students)

{-
  2.3.
  - Write a function that returns a list of matriculation numbers for a given
    study level, sorted by average grade in descending order.
    rankedStudents :: Level -> [Student] -> [String]
-}

rankedStudents :: Level -> [Student] -> [String]
rankedStudents lvl = map studentId . sortBy ((flip . comparing) avgGrade) . filter ((==lvl) . level)

{-
  2.4.
  - Write a function
    addStudent :: Student -> [Student] -> [Student]
    that adds a student to a list of students. If a student with an identical
    matriculation number already exists in the list, the function should return an
    error. 
-}

addStudent :: Student -> [Student] -> [Student]
addStudent x xs = if studentId x `notElem` map studentId xs then x:xs else error "student already exists"

-- EXERCISE 03 =======================================================================

{-
  3.1.
  - Define your own parametrized type 'MyTriplet' that contains the values of
    three different types. Do this using a record.
  - Define a function 
    toTriplet :: MyTriplet a b c -> (a, b, c)
    that converts a 'MyTriplet' value into an ordinary triplet.
-}

data MyTriplet a b c = MyTriplet (a, b, c)
type IntStrChrTriplet = MyTriplet Int String Char

toTriplet :: MyTriplet a b c -> (a, b, c)
toTriplet (MyTriplet (a, b, c)) = (a, b, c)

-- testing data:
testTriplet :: IntStrChrTriplet
testTriplet = MyTriplet (1, "str", 'a')

{-
  3.2.
  - Define a function (Employee - salary :: Maybe Double, name :: String) deriving Show
    totalSalaries :: [Employee] -> Double
    that sums the known salaries of employees (salaries that are not 'Nothing').
-}

data Employee = Employee
  { name   :: String
  , salary :: Maybe Double
  } deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries xs = sum $ map (fromMaybe 0 . salary) xs

-- testing data:
-- employees = [(Employee "Mate" (Just 1)), (Employee "Marko" Nothing), (Employee "Marina" (Just 42))]

{-
  3.3.
  - Write a function 'addStudent2' that works like 'addStudent' from problem 2.4
    but returns a 'Maybe' type instead of an error.
    addStudent2 :: Student -> [Student] -> Maybe [Student]
  - Write 'addStudent3' that returns an 'Either'.
-}

addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 x xs = if studentId x `notElem` map studentId xs then Just $ x:xs else Nothing

addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 x xs = if studentId x `notElem` map studentId xs then Right $ x:xs else Left "student already exists"