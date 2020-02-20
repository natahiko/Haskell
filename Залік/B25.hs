{-# OPTIONS_GHC -Wall #-}
module B25 where

seqDulb :: IO()
seqDulb = do putStr "Filenames> "
             line <- getLine
             file <- readFile line
             let res = getRes file
             writeFile "stdout.txt" res

getRes :: String -> String
getRes xs = unlines (unic (lines xs))
--getRes xs = unlines (reverse (lines xs))

unic :: (Ord a) => [a] -> [a]
unic (x:xs) | elem x xs = unic xs
            | otherwise = [x] ++ unic xs
unic [] = []

split :: Char -> String -> [String]
split _ [] = [""]
split ch (c:cs) | c == ch  = "" : rest
                | otherwise = (c : head rest) : tail rest
    where rest = split ch cs
