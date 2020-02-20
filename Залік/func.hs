{-# OPTIONS_GHC -Wall#-}
module Func where

split :: Char -> String -> [String]
split _ [] = [""]
split ch (c:cs) | c == ch  = "" : rest
                | otherwise = (c : head rest) : tail rest
    where rest = split ch cs

------------------------------------------------------------------------------

sort :: (Ord a) => [a] -> [a]
sort xs = sort2 xs 0

sort2 :: (Ord a) => [a] -> Int -> [a]
sort2 xs i | i == (length xs) = xs
           | otherwise = sort2 (iter xs) (i + 1) 

iter :: (Ord a) => [a] -> [a]
iter (x:(y:xs)) | x > y = (y : (iter (x:xs)))
                | otherwise = (x : (iter (y:xs)))
iter x = x

-----------------------------------------------------------------------------

unic :: (Ord a) => [a] -> [a]
unic (x:xs) | elem x xs = unic xs
            | otherwise = [x] ++ unic xs
unic [] = []
