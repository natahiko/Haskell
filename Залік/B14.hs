{-# OPTIONS_GHC -Wall #-}
module B14 where

primeCnt :: [Int] -> [Int]
primeCnt xs = [x | x<- xs, isSimple x]

isSimple :: Int -> Bool
isSimple n = let ys = [2..(div n 2)]
                 bol = or [mod n x == 0 | x<-ys]
             in not bol