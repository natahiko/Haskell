{-# OPTIONS_GHC -Wall #-}
module Shkarovska02 where

-- ������ 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl = foldl (+) 0
  
-- ������ 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr  = foldr (*) 1

-- ������ 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- ������ 4 -----------------------------------------
sortInsert :: [Int] -> [Int]
sortInsert = foldl insert []

insert :: [Int] -> Int -> [Int]
insert xs v = (filter (<v) xs) ++ [v] ++ (filter (>=v) xs)

-- ������ 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = [x | x<-[0..(length xs - 1)], p (xs!!x)]

-- ������ 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = map reverse xss

-- ������ 7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (\x -> not (elem x ['0'..'9'])) xs

-- ������ 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = length([x|x<-ps, x v])

-- ������ 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas = iterate nextPas [1] 

nextPas :: [Integer]->[Integer]
nextPas xs = [1] ++ [ xs!!(x-1)+xs!!x | x<-[1..(length xs -1)]] ++ [1]

-- ������ 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]
