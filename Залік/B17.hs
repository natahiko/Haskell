{-# OPTIONS_GHC -Wall#-}
module B17 where

import Data.List
import Data.Char

lequation :: IO()
lequation = do putStr "Input numbers> "
               x <- getLine
               putStrLn (getRes x)

getRes :: String -> String
getRes x = case (getNumbers x) of
        Just (a,b,c) -> if a==0 then (if c-b==0 then "many" else "no")
                        else (show (calc a b c))
        _ -> "error"

calc :: Double -> Double -> Double -> Double
calc a b c = (c-b)/a

getNumbers :: String -> Maybe (Double, Double, Double)
getNumbers xs = let ys = split ' ' xs
                    ys2 = [y|y<-ys, y/=""]
				in if (length ys2 < 3) then Nothing 
                   else case (getNumber (ys2!!0)) of
    Just a -> case (getNumber (ys2!!1)) of
        Just b -> case (getNumber (ys2!!2)) of
            Just c -> Just (a,b,c)
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

getNumber :: String -> Maybe Double
getNumber xs | elem '.' xs = if (head xs == '.') then getNumber (tail xs) 
                             else if ((xs!!(length xs - 1)) == '.' ) then getNumber [xs!!n|n<-[0..(length xs - 2)]]
                             else getDouble xs
             | otherwise  = getInt xs

getDouble :: String -> Maybe Double
getDouble xs = let y = split '.' xs
               in if (length y < 2) then Nothing 
                  else case (getInt (y!!0)) of
    Just a -> case (getDo (y!!1)) of
        Just b -> Just (a+b)
        _ -> Nothing
    _ -> Nothing

getDo :: String -> Maybe Double
getDo xs = let bol = and [isDigit x|x<-xs]
           in if bol then Just (getDo2 (xs) 1)
              else Nothing

getDo2 :: String -> Integer -> Double
getDo2 (x:xs) n = ((findInt x)/(10^n)) + (getDo2 xs (n+1))
getDo2 [] _ = 0

getInt :: String -> Maybe Double
getInt xs = let bol = and [isDigit x|x<-xs]
            in if bol then Just (getInt2 (reverse xs) 0)
               else Nothing

getInt2 :: String -> Integer -> Double
getInt2 (x:xs) n = (findInt x)*(10^n) + (getInt2 xs (n+1))
getInt2 [] _ = 0

findInt ::  Char -> Double 
findInt x = case x of
  '1'-> 1.0
  '2'-> 2.0
  '3'-> 3.0
  '4'-> 4.0
  '5'-> 5.0
  '6' -> 6.0
  '7' -> 7.0
  '8' -> 8.0
  '9'-> 9.0
  _ -> 0.0

split :: Char -> String -> [String]
split _ [] = [""]
split ch (c:cs) | c == ch  = "" : rest
                | otherwise = (c : head rest) : tail rest
    where rest = split ch cs