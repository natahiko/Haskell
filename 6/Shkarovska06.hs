{-# OPTIONS_GHC -Wall #-}
module Shkarovska06 where

import Data.List

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

------------------------------------ Задача 1 ----------------------------------------------------
isOrdinary :: Graph -> Bool 
isOrdinary gr | ([unic x|x<-gr])/=gr = False
              | otherwise = and [check gr x | x<-[0..(length gr - 1)]]

check :: Graph -> Int -> Bool
check gr x | x >= (length gr) = False
           | elem x (gr!!x) = False
           | length (gr!!x) < 1 = True
           | otherwise = and [if y>=(length gr) then False else elem x (gr!!y) | y<-gr!!x]

------------------------------------ Задача 2 ----------------------------------------------------
fromGraph :: Graph -> GraphS 
fromGraph gr | not (isOrdinary gr) = (0,[])
             | otherwise = (length gr - 1, [(x,y)|x<-[x|x<-[0..(length gr - 1)]], y<-(gr!!x)])

------------------------------------ Задача 3 ----------------------------------------------------
toGraph :: GraphS -> Graph 
toGraph (l, pairs) = [[b | (a,b)<-pairs, a==x] | x<-[0..l]]

------------------------------------ Задача 4 ----------------------------------------------------
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay gr a b = let res = getNext gr b [[a]]
                  in if res==[] then [] else res!!0

getNext :: Graph -> Int -> [[Int]] -> [[Int]]
getNext _ _ [] = []
getNext gr b xs = let res = [n | n<-xs, n!!(length n-1)==b]
                  in if length res > 0 then res -- ++ getNext gr b (stepW gr xs)
                       else getNext gr b (stepW gr xs)

stepW :: Graph -> [[Int]] -> [[Int]]
stepW gr (w:ws) = let adj = gr!!(w!!(length w - 1))
                  in [w++[x] | x<-adj, not (elem x w)] ++ stepW gr ws
stepW _ [] = []

------------------------------------ Задача 5 ----------------------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr = and [not (null (shortWay gr 0 x)) | x<-[1..(length gr - 1)]]

------------------------------------ Задача 6 ----------------------------------------------------
components :: Graph -> [[Int]] 
components gr | isConnecting gr = [[0..(length gr - 1)]]
              | otherwise = unic2 [sort (allPos gr ([x],[])) | x<-[0..(length gr - 1)]]

allPos :: Graph -> ([Int],[Int]) -> [Int]
allPos gr (ns,os) | ns==[] = os
                  | otherwise = unic (allPos gr (oneStep gr (ns,os)))

cond :: ([Int],[Int]) -> Bool
cond (ns,_) = ns==[]

oneStep :: Graph -> ([Int],[Int]) -> ([Int],[Int])
oneStep gr (ns, os) = let old = ns ++ os
                          ns1 = unic (concat [gr!!x | x<-ns])
                          new = [n | n<-ns1, not (elem n os)]
                      in (new, old)
unic :: [Int] -> [Int]
unic (x:xs) | elem x xs = unic xs
            | otherwise = [x] ++ unic xs
unic [] = []
unic2 :: [[Int]] -> [[Int]]
unic2 (x:xs) | elem x xs = unic2 xs
             | otherwise = [x] ++ unic2 xs
unic2 [] = []

------------------------------------ Задача 7 ----------------------------------------------------
eccentricity :: Graph -> Int -> Int 
eccentricity gr v = maximum [length (shortWay gr v x) - 1 | x<-[0..(length gr - 1)]]

------------------------------------ Задача 8 ----------------------------------------------------
findDiameter :: Graph -> Int 
findDiameter gr = maximum [eccentricity gr x | x<-[0..(length gr - 1)]]

findRadius :: Graph -> Int 
findRadius gr = minimum [eccentricity gr x | x<-[0..(length gr - 1)]]

------------------------------------ Задача 9 ----------------------------------------------------
findCenter :: Graph -> [Int] 
findCenter gr = [x | x<-[0..(length gr - 1)], (eccentricity gr x) == findRadius gr]

------------------------------------ Задача 10 ----------------------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr a b = getNext gr b [[a]]

--------------------- Кортеж граф - список -------
gr1S, gr2S, gr3S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])
gr3S = (7,[(0,1),(1,0),(1,3),(1,7),(2,3),(3,1),(3,7),(3,2),(4,5),(4,6),(5,4),(5,6),(6,5),(6,4),(7,1),(7,3)])

gr1, gr2, gr3:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]
gr3 = [[1],[0,3,7],[3],[1,7,2],[5,6],[4,6],[5,4],[1,3]]