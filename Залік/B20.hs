{-# OPTIONS_GHC -Wall #-}
module B20 where

bagSubbag :: String -> String -> Bool
bagSubbag (x:xs) w | elem x w = bagSubbag xs (delOne w x)
                   | otherwise = False
bagSubbag [] _ = True 

delOne :: String -> Char -> String
delOne (x:xs) w | x==w = xs
                | otherwise = [x] ++ delOne xs w
delOne [] _ = ""