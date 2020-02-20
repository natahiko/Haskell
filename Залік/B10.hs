{-# OPTIONS_GHC -Wall #-}
module B10 where

lastTail :: String -> String
lastTail xs = let tails = getTails xs
              in if tails==[] then "" 
                 else getMax tails (tails!!0)

getTails :: String -> [String]
getTails xs = [drop x xs | x<-[0..(length xs - 1)]]

getMax :: [String] -> String -> String
getMax (x:xs) m | m>x = getMax xs m
                | otherwise = getMax xs x
getMax [] m = m