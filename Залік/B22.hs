{-# OPTIONS_GHC -Wall #-}
module B21 where

mainF :: String -> String -> String
mainF xs ys = merge (unic xs) (unic ys)

merge :: String -> String -> String
merge (x:xs) ys | elem x ys = [x] ++ (merge xs ys)
                | otherwise = merge xs ys
merge [] _ = []


unic :: (Ord a) => [a] -> [a]
unic (x:xs) | elem x xs = unic xs
            | otherwise = [x] ++ unic xs
unic [] = []