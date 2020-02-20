{-# OPTIONS_GHC -Wall #-}
module B26 where

seqDulb :: IO()
seqDulb = do putStr "Filename> "
             line <- getLine
             file <- readFile line
             let res = getRes file
             writeFile "stdout.txt" res

getRes :: String -> String
getRes xs = let lins = lines xs
            in unlines [clear x|x<-lins]

clear :: String -> String
clear xs = let ys = split ' ' xs
               un = unic ys
           in concat [x++" "| x<-un]

unic :: (Ord a) => [a] -> [a]
unic (x:xs) | elem x xs = unic xs
            | otherwise = [x] ++ unic xs
unic [] = []

split :: Char -> String -> [String]
split _ [] = [""]
split ch (c:cs) | c == ch  = "" : rest
                | otherwise = (c : head rest) : tail rest
    where rest = split ch cs
