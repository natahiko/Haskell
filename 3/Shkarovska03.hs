{-# OPTIONS_GHC -Wall #-}
module Shkarovska03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
type ConfigС = (Int, Int, [Int])

-- Задача 1 -----------------------------------------------------------------------------
isPrefix :: String -> String -> Bool 
isPrefix xs ys | xs=="" = True
               | length xs > length ys = False
               | (head xs)==(head ys) = isPrefix (tail xs) (tail ys)
               | otherwise = False

-- Задача 2 -----------------------------------------------------------------------------
substitute :: Substitution -> Int -> String -> String
substitute (sub1, sub2, _) i w = (take i w) ++ sub2 ++ 
                               (drop (i+length sub1) w)

-- Задача 3-----------------------------------------------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition w sub@(sub1,_, _) | sub1=="" = [(sub, x) | x<-[0..(length w)]]
                                | otherwise = [(sub, x) | x<-[0..(length w)], isPrefix sub1 (drop x w)]

-- Задача 4 -----------------------------------------------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll algo w = concat (map (findPosition2 w) algo)

-- Задача 5 -----------------------------------------------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (bt,st,w) | not bt = (bt,st,w)
                     | otherwise = (not sub3, st+1,substitute sub i w)
                       where (sub@(_,_,sub3),i) = head (findAll algo w)

-- Задача 6 -----------------------------------------------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word | m<1 = Nothing
                  | otherwise = if bs then evalA algo (m-1) w else Just w
                                where (bs,_,w) = stepA algo (True,0,word)

-- Задача 7 -----------------------------------------------------------------------------
maximReg :: Program -> Int
maximReg pr = maximum [maxComReg x | x<-pr]

maxComReg::Command->Int
maxComReg (Z a) = a
maxComReg (S a) = a
maxComReg (T a b) = max a b
maxComReg (J a b _) = max a b

-- Задача 8 -----------------------------------------------------------------------------
ini :: Program -> [Int] -> [Int] 
ini pr ir | length ir >= maximReg pr = ir
          | otherwise = ir ++ [0*x | x<-[1..(maximReg pr - length ir)]]

upd :: [Int] -> Int -> Int-> [Int]
upd req r v = (take r req) ++ [v] ++ (drop (r+1) req) 

-- Задача 9 -----------------------------------------------------------------------------
stepC :: Program -> ConfigС -> ConfigС
stepC pr conf@(nm,_,_) | nm>=length pr = conf
                       | otherwise = stepC2 (pr!!(nm-1)) conf

stepC2 :: Command -> ConfigС -> ConfigС
stepC2 (Z a) (nm,st,rg) = (nm+1, st+1, upd rg (a-1) 0)
stepC2 (S a) (nm,st,rg) = (nm+1, st+1, upd rg (a-1) (rg!!(a-1) + 1))
stepC2 (T a b) (nm,st,rg) = (nm+1, st+1, upd rg (a-1) b)
stepC2 (J a b c) (nm,st,rg) | rg!!(a-1)==rg!!(b-1) = (c, st+1, rg)
                            | otherwise = (nm+1, st+1, rg)

-- Задача 10 -----------------------------------------------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC pr mx ir = evalC2 pr mx (1,0,ini pr ir)

evalC2 :: Program -> Int -> ConfigС -> Maybe Int
evalC2 pr mx conf@(nm,st,rg) | mx<=st = Nothing
                             | nm >= length pr = Just (rg!!(length rg - 1))
                             | otherwise = evalC2 pr mx (stepC pr conf)

---------------------Тестові дані - Програми МНР -----------------------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]
