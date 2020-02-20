{-# OPTIONS_GHC -Wall #-}
module B7 where

seqSymb :: IO()
seqSymb = do putStr "Filename> "
             line <- getLine
             file <- readFile line
             let res = getRes file
             writeFile "stdout.txt" res

getRes :: String -> String
getRes xs = unlines ([clearLine x| x<-(lines xs)])

clearLine :: String -> String
clearLine [] = []
clearLine xs | length xs < 3 = xs
             | (xs!!0)==(xs!!1) && (xs!!1)==(xs!!2) = let (a,b) = clear xs (xs!!0) 0
                                                      in a ++ (clearLine b)
             | otherwise = [head xs] ++ (clearLine (tail xs))

clear :: String -> Char -> Integer -> (String,String)
clear (x:xs) y n | y==x = clear xs y (n+1)
                 | otherwise = ("("++(show n)++")"++[y], [x]++xs)
clear [] y n = ("("++(show n)++")"++[y],"")
