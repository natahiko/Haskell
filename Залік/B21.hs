{-# OPTIONS_GHC -Wall #-}
module B21 where

bagUnion :: String -> String -> String
bagUnion (x:xs) ys = let am1 = (amount xs x)+1
                         am2 = amount ys x
                         nxs = delete xs x
                         nys = delete ys x
                         res = if am1>am2 then (getAm am1 x)
                               else (getAm am2 x)
                     in res ++ (bagUnion nxs nys)
bagUnion [] ys = ys

amount :: String -> Char -> Int
amount (x:xs) y | y==x = 1 + (amount xs y)
                | otherwise = amount xs y
amount [] _ = 0

delete :: String -> Char -> String
delete (x:xs) y | x==y = delete xs y
                | otherwise = [x]++(delete xs y)
delete [] _ = []

getAm :: Int -> Char -> String
getAm n ch = [ch | _<-[1..n]]