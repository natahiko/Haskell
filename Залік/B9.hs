{-# OPTIONS_GHC -Wall #-}
module B9 where

intToString :: Int -> Int -> String
intToString n m = let res = findRes n m
                  in reverse res

findRes :: Int -> Int -> String
findRes n m | m < n = [toChar (m `mod` n), toChar (div m n)]
            | otherwise = [toChar (m `mod` n)] ++ (findRes n (div m n))

toChar :: Int -> Char 
toChar x = case x of
  0-> '0'
  1-> '1'
  2-> '2'
  3-> '3'
  4-> '4'
  5-> '5'
  6 -> '6'
  7 -> '7'
  8 -> '8'
  9 -> '9'
  10 -> 'a'
  11 -> 'b'
  12 -> 'c'
  13 -> 'd'
  14 -> 'e'
  15 -> 'f'  
  _ -> '0'