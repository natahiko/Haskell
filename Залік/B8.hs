{-# OPTIONS_GHC -Wall #-}
module B8 where

import Data.Char

sum2 :: IO()
sum2 = do putStr "xs> "
          xs <- getLine
          putStr "ys> "
          ys <- getLine
          putStrLn (getRes xs ys)

getRes :: String -> String -> String
getRes xs ys = case (getNumber xs) of
    Just a -> case(getNumber ys) of 
        Just b -> (show (a+b))
        _ -> "error"
    _ -> "error"

getNumber :: String -> Maybe Int
getNumber xs = let xs2 = removeSpaces xs
                   isMin = if xs2==[] then False else (head xs2)=='-'
                   nxs = if xs2==[] then [] else if ((head xs2)=='-') || ((head xs2)=='+') 
                         then reverse (drop 1 xs2) else reverse xs2
                   cor = and [isDigit x| x<- nxs]
               in if not cor then Nothing 
                  else if isMin then Just ((getInt nxs 0)*(-1)) else Just (getInt nxs 0)

getInt :: String -> Int -> Int
getInt (x:xs) n = (findInt x)*(10^n) + (getInt xs (n+1))
getInt [] _ = 0

removeSpaces :: String -> String
removeSpaces (x:xs) | x==' ' = removeSpaces xs
                    | xs == [] = [x]
                    | xs!!(length xs - 1)==' ' = removeSpaces [x]++[xs!!y|y<-[0..(length xs - 2)]]
                    | otherwise = [x]++xs
removeSpaces [] = []

findInt ::  Char -> Int
findInt x = case x of
  '1'-> 1
  '2'-> 2
  '3'-> 3
  '4'-> 4
  '5'-> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9'-> 9
  _ -> 0
