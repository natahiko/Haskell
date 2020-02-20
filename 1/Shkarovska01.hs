{-# OPTIONS_GHC -Wall #-}
module Shkarovska01 where

-- Задача 1 -----------------------------------------
power3 :: [Integer]
power3 = [x*x*x | x <- [1..]]

-- Задача 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3^x | x <- [1..]]

-- Задача 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [3^x | x <- [1..n]]

-- Задача 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n | m<0 = 0
			 | otherwise = sum [m^x | x <- [1..n]]

-- Задача 5 -----------------------------------------
lessMe :: [Int] -> [Int]
lessMe xs = let f x xs | xs==[] = 0
					   | head xs < x = 1 + f x (tail xs)
					   | otherwise = f x (tail xs)
			in [(f x xs) | x <- xs]

-- Задача 6 -----------------------------------------
frequency :: [Int] -> [(Int,Int)]
frequency xs = [(x, (f x xs)) | x <- (uniq xs)]
				where uniq xs | xs==[] = []
						      | otherwise = head xs : uniq (filter (head xs/=) xs)
				      f x xs = length [y | y <- xs, y==x]

-- Задача 7 -----------------------------------------
hailstone :: Int -> Int
hailstone n | even n = n `div` 2
			| otherwise = n*3+1

-- Задача 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n | n==1 = [n]
		  | otherwise = [n] ++ hailSeq (hailstone n)

-- Задача 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1..]]

-- Задача 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = head [head x | x <- allHailSeq, length x == l]
