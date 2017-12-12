module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\f n -> if n==0 then 1 else n * f(n-1))

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = fix (\f l -> case l of { [] -> 0; x:xs -> x + f xs } )

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' = fix (\f acc n -> if n==0 then acc else let x = acc * n in x `seq` f x (n-1)) 1

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' = fix (\f acc l -> case l of { [] -> acc; x:xs -> let s = x + acc in s `seq` f s xs } ) 0

nats :: [Integer]
nats = fix (\f n -> n : f(n+1)) 1

map' :: (a -> b) -> [a] -> [b]
map' = fix (\f fun l -> case l of { [] -> []; x:xs -> fun x : f fun xs } )

zip' :: [a] -> [b] -> [(a, b)]
zip' = fix (\f l1 l2 -> case (l1, l2) of { ([], _) -> []; (_, []) -> []; (x:xs, y:ys) -> (x, y) : f xs ys } )

-- Task 02
subsets :: Eq a => Int -> [a] -> [[a]]
subsets n xs = f n [] $ nub xs
    where   f 0  _     _     = [[]]
            f 1 acc   [x]    = [x:acc]
            f _  _    [x]    = []
            f 1 acc l@(x:xs) = (x:acc) : f 1 acc xs
            f j acc l@(x:xs) = if length l < j then [] else f (j-1) (x:acc) xs ++ f j acc xs

-- order of elements is a bit off, use 'map (sort)' if testing on Ord types

partitions :: [a] -> [[[a]]]
partitions = undefined

-- Task 03
-- wip - needs better logic
permutations' :: [a] -> [[a]] 
permutations' list = f [] list
    where   permutationsOfThree a b c = [a,b,c] : [a,c,b] : [b,a,c] : [b,c,a] : [c,a,b] : [c,b,a] : []
            f [] [x] = [[x]]
            f [] [x0,x1] = (x0:x1:[]) : (x1:x0:[]) : []
            f [] (x0:xs) = f (x0:[]) xs
            f (a0:acc) [x] = (a0:x:acc) : (x:a0:acc) : []
            f (a0:acc) [x0,x1] = map (\t -> t ++ acc) $ permutationsOfThree a0 x0 x1
            f (a0:acc) (x0:x1:xs) = concat . map (\[t0,t1,t2] -> f (t0:t1:acc) (t2:xs)) $ permutationsOfThree a0 x0 x1