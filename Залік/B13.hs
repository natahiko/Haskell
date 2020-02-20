{-# OPTIONS_GHC -Wall #-}
module B13 where

stringToInt :: Integer -> String -> Maybe Integer
stringToInt n xs | notSuit n xs = Nothing
                 | otherwise = let l = (length xs - 1)
                               in Just (calc n xs l)

calc :: Integer -> String -> Int -> Integer
calc n xs k | k==0 = findInt (head xs)
            | otherwise = (findInt (head xs))*(n^k) + (calc n (tail xs) (k-1))

notSuit::Integer->String->Bool
notSuit n xs = or [(findInt x)>= n|x<-xs]

findInt ::  Char -> Integer 
findInt x = case x of
  '0'-> 0
  '1'-> 1
  '2'-> 2
  '3'-> 3
  '4'-> 4
  '5'-> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9'-> 9
  'a' -> 10
  'b' -> 11
  'c' -> 12
  'd' -> 13
  'e' -> 14
  'f' -> 15  
  _ -> 0

