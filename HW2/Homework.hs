module Homework where
--
import Data.List
import Data.Char
--

-- Task 01
toRNA :: String -> String
toRNA []     = []
toRNA (x:xs) = cRNANucleotide (toUpper x) : toRNA xs
    where cRNANucleotide 'G' = 'C'
          cRNANucleotide 'C' = 'G'
          cRNANucleotide 'T' = 'A'
          cRNANucleotide 'A' = 'U'
          cRNANucleotide  _  = error "Invalid DNA sequence" 

--

-- Task 02
multiply :: Int -> Int -> Int
multiply x y = (signum y) * (sum $ replicate (abs y) x)

-- NOTE: the multiply with signum was added to support negative numbers
--       without the need to have different logic for those situations

multiply' :: Int -> Int -> Int  -- accumulator style multiply
multiply' x y = (signum y) * innerMultiply x (abs y) 0
    where innerMultiply a 0 result = result
          innerMultiply a b result = 
            let i = b - 1
                r = result + a
            in i `seq` r `seq` innerMultiply a i r


divide :: Int -> Int -> Int  -- accumulator style
divide x y = (signum x) * (signum y) * innerDivide (abs x) (abs y) 0
    where innerDivide a b result
            | a < b     = result
            | otherwise = 
                let step = a - b
                    inc = result + 1
                in step `seq` inc `seq`innerDivide (a-b) b inc


greatestCD :: Int -> Int -> Int
greatestCD x y
    | abs y > abs x  = greatestCD y x
    | x `mod` y == 0 = abs y
    | otherwise      = greatestCD y $ mod x y 

-- Note that by definition: gcd(a,b) = gcd(-a,b) = gcd(a,-b) = gcd(-a,-b)

--

-- Task 03
numberToWords :: Int -> String
numberToWords 0 = "zero" 
numberToWords 1 = "one"
numberToWords 2 = "two"
numberToWords 3 = "three"
numberToWords 4 = "four"
numberToWords 5 = "five"
numberToWords 6 = "six"
numberToWords 7 = "seven"
numberToWords 8 = "eight"
numberToWords 9 = "nine"
numberToWords 10 = "ten"
numberToWords 11 = "eleven"
numberToWords 12 = "twelve"
numberToWords 13 = "thirteen"
numberToWords 14 = "fourteen"
numberToWords 15 = "fifteen"
numberToWords 16 = "sixteen"
numberToWords 17 = "seventeen"
numberToWords 18 = "eighteen"
numberToWords 19 = "nineteen"
numberToWords 20 = "twenty"
numberToWords 30 = "thirty"
numberToWords 40 = "fourty"
numberToWords 50 = "fifty"
numberToWords 60 = "sixty"
numberToWords 70 = "seventy"
numberToWords 80 = "eighty"
numberToWords 90 = "ninety"
numberToWords x
    | x < 0         = "minus " ++ numberToWords (-x)
    | x >= 1000000  = concatWith x 1000000 "milion"
    | x >= 1000     = concatWith x 1000 "thousand"
    | x >= 100      = concatWith x 100 "hundred"
    | x > 20        = numberToWords (x `div` 10 * 10) 
                        ++ if (x `mod` 10 /= 0) 
                            then "-" ++ numberToWords(x `mod` 10)
                            else ""
    where concatWith y num name = numberToWords (y `div` num) ++ " " ++ name
                                    ++ if (y `mod` num /= 0) 
                                        then " and " ++ numberToWords(y `mod` num)
                                        else ""


-- Task 04
undefined' :: a
undefined' = error "undefined" -- interesting :) 