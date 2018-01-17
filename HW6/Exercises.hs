{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.Maybe
import Control.Exception
import Data.Char
import Data.List
import qualified Data.Map as M
import Control.Monad
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.FilePath
import System.Random
import System.FilePath
import Data.Set (Set)

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs
    http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 10-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-10.lhs

-- EXERCISE 01 =======================================================================

data Sex = Male | Female deriving (Show,Read,Eq,Ord)

data Person2 = Person2 {
    personId2 :: String,
    forename2 :: String,
    surname2  :: String,
    sex2      :: Sex,
    mother2   :: Maybe Person2,
    father2   :: Maybe Person2,
    partner2  :: Maybe Person2,
    children2 :: [Person2] 
} deriving (Show,Read,Eq,Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) [jack]
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane, mary]
mary = Person2 "234" "Mary" "Doe" Female (Just ann) Nothing Nothing []
jack = Person2 "345" "Jack" "Doe" Male Nothing Nothing Nothing []

eqById :: Person2 -> Person2 -> Bool
eqById p = (==personId2 p) . personId2

{-
  1.2.
  - Define a function
    parentCheck :: Person2 -> Bool
    that checks whether the given person is one of the children of its parents.
-}

parentCheck :: Person2 -> Bool
parentCheck p = personId2 p `elem` concatMap (map personId2 . fromMaybe [] . fmap children2) [mother2 p, father2 p]

{-
  1.3.
  - Define a function
    sister :: Person2 -> Maybe Person2
    that returns the sister of a person, if such exists.
-}

sister :: Person2 -> Maybe Person2
sister p = 
  let femaleSiblings = filter ((==Female) . sex2) . siblings $ p
  in if null femaleSiblings then Nothing else Just $ head femaleSiblings
  where siblings p = filter (not . eqById p) . concatMap (fromMaybe [] . fmap children2) $ [mother2 p, father2 p] 

{-
  1.4.
  - Define a function that returns all descendants of a person.
    descendant :: Person2 -> [Person2]
-}

descendant :: Person2 -> [Person2]
descendant p = let children = children2 p in children ++ concatMap descendant children

-- EXERCISE 02 =======================================================================

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord,Eq)

{-
  2.1.
  - Define
    listHead :: MyList a -> Maybe a
-}

listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons a _) = Just a

{-
  2.2.
  - Define a function that works like 'map' but works on a 'MyList' type:
    listMap :: (a -> b) -> MyList a -> MyList b
-}

listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty       = Empty
listMap f (Cons x xs) = f x `Cons` listMap f xs

-- EXERCISE 03 =======================================================================

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

intTree :: Tree Int
intTree = Node 4 (Node 1 Null (Node 3 (Node 2 Null Null) Null)) (Node 8 (Node 6 Null (Node 7 Null Null)) Null)

{-
  3.1.
  - Define a function
    treeMax :: Ord a => Tree a -> a
    that finds the maximum element in a tree. Return an error if the tree is
    empty.
-}

treeMax :: Ord a => Tree a -> a
treeMax Null                = error "empty Tree"
treeMax (Node x Null Null)  = x
treeMax (Node x lxs  Null)  = max x $ treeMax lxs
treeMax (Node x Null rxs )  = max x $ treeMax rxs
treeMax (Node x lxs  rxs )  = max x $ max (treeMax lxs) (treeMax rxs)

{-
  3.2.
  - Define a function
    treeToList :: Ord a => Tree a -> [a]
    that will collect in a list all elements from inner nodes of a tree by doing
    an in-order (left-root-right) traversal.
-}

treeToList :: Ord a => Tree a -> [a]
treeToList Null               = []
treeToList (Node x lxs  rxs ) = treeToList lxs ++ [x] ++ treeToList rxs

{-
  3.3.
  - Define a function to prune the tree at a given level (root has level 0).
    levelCut :: Int -> Tree a -> Tree a
-}

levelCut :: Int -> Tree a -> Tree a
levelCut _ Null             = Null
levelCut 0 (Node x _   _  ) = Node x Null Null
levelCut i (Node x lxs rxs) = Node x (levelCut (i-1) lxs) (levelCut(i-1) rxs)

-- EXERCISE 04 =======================================================================

treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t  

{-
  4.1.
  - Define a function that converts a list into a sorted tree.
    listToTree :: Ord a => [a] -> Tree a
-}

listToTree :: Ord a => [a] -> Tree a
listToTree = foldr treeInsert Null

{-
  4.2.
  - Using 'listToTree' and 'treeToList' defined previously, define these two 
    functions, define:
    sortAndNub :: Ord a => [a] -> [a]
-}

sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define an 'Eq' instance for the 'Weekday' type that works like (==), except
    that two Fridays are never identical.
-}

data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = False
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

{-
  5.2.
  - Define 'Person' as an instance of 'Show' type class so that instead of the
    values of partners and children only the respective person names are shown,
    which will enable the print out of an infinite structure of this type.
-}

data Person = Person
  { idNumber :: String
  , forename :: String
  , surname  :: String
  , sex      :: Sex
  , age      :: Int
  , partner  :: Maybe Person
  , children :: [Person]
  } deriving (Read)

instance Show Person where
  show p = "Person {idNumber = \"" ++ idNumber p ++ "\", \
            \forename = \"" ++ forename p ++ "\", \
            \surname = \"" ++ surname p ++ "\", \
            \sex = " ++ show (sex p) ++ ", \
            \age = " ++ show (age p) ++ ", \
            \partner = " ++ showPersonPartial (partner p) ++ ", \
            \children = " ++ showChildren (children p) ++ "}"
    where 
      showChildren [] = "[]"
      showChildren cs = intercalate " , " $ map (showPersonPartial . Just) cs
      showPersonPartial Nothing  = "None"
      showPersonPartial (Just p) = 
        "PersonPartial {forename = \"" ++ forename p ++ "\", surname = \"" ++ surname p ++ "\"}"

pero  = Person "2323" "Pero"  "Perić" Male   45 (Just ana)   [marko]
ana   = Person "3244" "Ana"   "Anić"  Female 43 (Just pero)  [marko,iva]
marko = Person "4341" "Marko" "Perić" Male   22 (Just maja)  []
maja  = Person "7420" "Maja"  "Majić" Female 20 (Just marko) []
iva   = Person "4642" "Iva"   "Ivić"  Female 16 Nothing      []


{-LECTURE 11-} -- http://www.fer.unizg.hr/_download/repository/puh-2016-lecture-11.lhs

-- EXERCISE 01 =======================================================================

{- DON'T FORGET TO WRITE TYPE SIGNATURES-}

{-
  1.1.
  - Define a 'main' function that reads in two strings and prints them out
    concatenated and reversed.
-}

main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  putStrLn . reverse $ s1 ++ s2

{-
  1.2.
  - Write a function 'threeNumbers' that reads in three numbers and prints out
    their sum.
-}

threeNumbers :: IO ()
threeNumbers = do
  x <- readLn :: IO Int
  y <- readLn :: IO Int
  z <- readLn :: IO Int
  print $ x + y + z

-- EXERCISE 02 =======================================================================
{-
  2.1.
  - Define a function 'threeStrings' that reads in three strings and outputs them
    to the screen as one string, while it returns its total length.
    treeStrings :: IO Int
-}

threeStrings :: IO Int
threeStrings = do 
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  let str = s1 ++ s2 ++ s3
  putStrLn str
  return $ length str

{-
  2.2.
  - Define a function 'askNumber9' that reads in a number and returns that number
    converted into an 'Int'. Input should be repeated until the user enters a
    number (a string containing only digits).
      askNumber9 :: IO Int
-}

askNumber9 :: IO Int
askNumber9 = do
  input <- getLine
  if any (not . isDigit) input then askNumber9 else return $ read input

{-
  2.3.
  - Define a function 'askUser m p' that returns an action that prints out 'm',
    reads in a string from the input, repeats the input until the input
    string satisfies the function 'p', and then returns the input string.
      askUser :: String -> (String -> Bool) -> IO String
  - Generalize this function to
      askUser' :: Read a => String -> (String -> Bool) -> IO a
-}

askUser :: String -> (String -> Bool) -> IO String
askUser m p = do 
  putStrLn m
  str <- getLine
  if p str then return str else askUser m p

-- call with cast, e.g. askUser' "aaa" (all isDigit) :: IO Int
askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = do 
  putStrLn m
  str <- getLine
  if p str then return $ read str else askUser' m p

{-
  2.4.
  - Define a function that reads in strings until the user inputs an empty
    string, and then returns a list of strings received as input.
      inputStrings :: IO [String]
-}

inputStrings :: IO [String]
inputStrings = buildString []
  where 
    buildString list = do 
      str <- getLine
      if str /= "" then buildString (str:list) else return list

-- EXERCISE 03 =======================================================================
{-
  3.1.
  - Define a function that reads in a number, then reads in that many
    strings, and finally prints these strings in reverse order.
-}

main31 :: IO ()
main31 = do 
  num <- readLn :: IO Int
  strs <- replicateM num getLine
  mapM_ putStrLn $ reverse strs

{-
  3.2.
  - Give recursive definitions for 'sequence' and 'sequence_'.
-}

{-
  3.3.
  - Give a recursive definitions for 'mapM' and 'mapM_'.
-}

{-
  3.4.
  - Define a function that prints out the Pythagorean triplets whose all sides
    are <=100. Every triplet should be in a separate line.
-}

-- EXERCISE 04 =======================================================================
{-
  4.1.
  - Define a function that removes from standard input every second line and
    prints the result to standard output.
      filterOdd :: IO ()
-}

filterOdd :: IO ()
filterOdd = interact $ unlines . map snd . filter (even . fst) . zip [0..] . lines

{-
  4.2.
  - Define a function that prefixes each line from standard input with a line
    number (number + space).
      numberLines :: IO ()
-}

numberLines :: IO ()
numberLines = interact $ unlines . map (\(i, l) -> (show i) ++ " " ++ l) . zip [0..] . lines

{- 4.3.
  - Define a function to remove from standard input all words from a given set of
    words.
      filterWords :: Set String -> IO ()
-}

filterWords :: Set String -> IO ()
filterWords = undefined

-- EXERCISE 05 =======================================================================
{-
  5.1.
  - Define a function
    wc :: FilePath -> IO (Int, Int, Int)
    that counts the number of characters, words, and lines in a file.
-}

wc :: FilePath -> IO (Int, Int, Int)
wc = undefined

{-
  5.2. 
  - Define a function
    copyLines :: [Int] -> FilePath -> FilePath -> IO ()
    that copies given lines from the first file into the second.
-}

copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines = undefined

-- EXERCISE 06 =======================================================================
{-
  6.1.
  - Define a function
      wordTypes :: FilePath -> IO Int
    to compute the number of distinct words in the given file.
-}

wordTypes :: FilePath -> IO Int
wordTypes = undefined

{-
  6.2.
  - Define a function 
      diff :: FilePath -> FilePath -> IO ()
    that takes two file names, compares their corresponding lines, and then
    outputs to standard output all lines in which the files differ. Lines should 
    be printed one below the other, prefixed with "<" for the first and ">" for
    the second file.
-}

diff :: FilePath -> FilePath -> IO ()
diff = undefined

{-
  6.3.
  - Define a function
      removeSpaces :: FilePath -> IO () 
    that removes trailing spaces from all lines in the given file.
    The function should change the original file.
-}

removeSpaces :: FilePath -> IO () 
removeSpaces = undefined

-- EXERCISE 07 =======================================================================
{-
  7.1.
  - Define a function
      fileHead :: IO ()
    that prints the first 'n' lines from a file. The name of the file and the
    number of lines are specified at the command line, e.g.:
      filehead -5 input.txt
    If the number of lines is missing, default to 10. If file name is missing,
    read from the standard input. If the file doesn't exist, print an error
    message and exit with failure using 'exitFailure' from 'System.Exit'.
-}

fileHead :: IO ()
fileHead = undefined

{-
  7.2.
  - Define a function
      sortFiles :: IO ()
    that sorts lines from multiple files and prints them to standard output.
    File names are provided at the command line.
    "sortFiles file1.txt file2.txt file3.txt"
    If any of the files does not exist, print an error message.
-}

sortFiles :: IO ()
sortFiles = undefined

-- EXERCISE 08 =======================================================================
{-
  8.1.
  - Define your own implementation of
      randoms' :: (RandomGen g, Random a) => g -> [a]
-}

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' = undefined

{-
  8.2.
  - Define a function
      randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
    that returns a list of randomly generated integer coordinates from within a
    given interval.
      randomPositions 0 10 0 10 => [(2,1),(4,3),(7,7),...
-}

randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
randomPositions = undefined