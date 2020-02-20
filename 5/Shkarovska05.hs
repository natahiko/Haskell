{-# OPTIONS_GHC -Wall #-}
module Shkarovska05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

----------------------- Задача 1 ------------------------------------
addOne :: String -> Char -> String  
addOne (x:xs) c | c==x = [x]++xs
                | c>x = if elem x xs then addOne xs c
                        else [x] ++ addOne xs c
                | otherwise = [c]++[x]++xs
addOne [] c = [c]

addAll :: String -> String -> String 
addAll st (w:ws) = addAll (addOne st w) ws
addAll st [] = st

addWithout :: String -> String -> String 
addWithout st w = addWithout2 [x|x<-st,not (x=='$')] w
addWithout2 :: String -> String -> String 
addWithout2 st (w:ws) | w=='$' = addWithout st ws
                     | otherwise = addWithout (addOne st w) ws
addWithout2 st [] = st

inter :: String -> String -> String 
inter st1 st2 = interBy st1 st2

interBy :: String -> String -> String 
interBy [] _ = []
interBy _ [] = []
interBy xs ys = [x | x <- xs, any ((==) x) ys]

----------------------- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String 
tkPredict (p:ps) n | n==(fst p) = snd p
                   | otherwise = tkPredict ps n
tkPredict [] _ = []

upPredict :: Predict -> Char -> String -> Predict 
upPredict pt@(p:ps) n st | n==(fst p) = [(n,st)]++ps
                         | n>(fst p) = [p]++(upPredict ps n st)
                         | otherwise = [(n,st)]++pt
upPredict [] _ _ = []

----------------------- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gr ctl w = let (_,_,res) = step gr ctl (w++"$",[fst (gr!!0)]++"$",Just [])
                 in res

step :: Grammar -> Control -> 
        (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step _ _ ("$","$", res) = ("","",res)
step _ _ (_,_,Nothing) = ("","",Nothing)
step gr ctl (inp, st,Just res) = let y = [n|((a,b),n)<-ctl,b==head inp && a==head st]
                                 in if head inp==head st then step gr ctl (tail inp, tail st,Just res)
        else if not (isUpper (head st)) then step gr ctl (inp,tail st,Just res)
        else if length y <= 0 then (inp, st, Nothing)
        else if (snd (gr!!(y!!0)))=="" && inp=="$" then 
            step gr ctl (inp, (snd (gr!!(y!!0)))++ tail st, Just (res++y))
        else if (snd (gr!!(y!!0)))=="" then 
            step gr ctl (tail inp, (snd (gr!!(y!!0)))++ st, Just (res))
        else step gr ctl (inp, (snd (gr!!(y!!0)))++ tail st, Just (res++y))

----------------------- Задача 4 ------------------------------------
first :: Predict -> String -> String
first _ [] = ""
first pFst st | not (isUpper (head st)) = [head st]
              | emp pFst st = let y = [b|(a,b)<-pFst, a==head st]!!0
                              in addAll y (first pFst (tail st))
              | otherwise = let y = [b|(a,b)<-pFst, a==head st]!!0
                            in if elem '$' y
                               then addWithout y (first pFst (tail st)) else y

emp :: Predict -> String -> Bool
emp _ [] = True
emp pFst st = let y = [b|(a,b)<-pFst, a==head st]!!0
              in if elem '$' y then True && (emp pFst (tail st))
                 else False


----------------------- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl _ [] _ = []
buildingControl gr pFst pNxt = let pairs = getPairs pFst
                               in [((a,b),(find1 gr (a,b)))|(a,b)<-pairs, b/='$']
                                  ++ (findz gr [a|(a,b)<-pairs, b=='$'] pNxt)

findz::Grammar->[Char]->Predict->Control
findz gr (x:xs) pNxt = let pairsz = getPairsZ pNxt x
                           zero = [n| n<-[0..(length gr -1)], (fst (gr!!n))==x && (snd (gr!!n))==""]!!0
                       in [((x,b),zero)|b<-pairsz] ++ (findz gr xs pNxt)
findz _ [] _ = []

getPairsZ:: Predict->Char->[Char]
getPairsZ pNxt x = let y = [b| (a,b)<-pNxt, a==x ]!!0
                   in y

find1::Grammar->(Char, Char)->Int
find1 gr (a,b) = let y = [n| n<-[0..(length gr -1)], (fst (gr!!n))==a && (snd (gr!!n))/=""]
                     yy = [n| n<-y, (snd (gr!!n))!!0==b]
                 in if length yy < 1 then y!!0 else yy!!0

getPairs :: Predict -> [(Char, Char)]
getPairs [] = []
getPairs pFst = let (a,b) = head pFst
                    y = [(a,x)|x<-b]
                in y ++ getPairs (tail pFst)

----------------------- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 [] _ _ = True
testingLL1 gr pFst pNxt = let now  = fst (gr!!0)
                              y = [b| (a,b)<-gr, a==now]
                          in (testFst now y)&&(test y) && (testingLL1 [(a,b)|(a,b)<-gr, a/=now] pFst pNxt)

testFst::Char->[String]->Bool
testFst w ("":xs) = (testFst w xs)
testFst w (x:xs) = w/=head x && (testFst w xs)
testFst _ [] = True

test:: [String] -> Bool
test st = let y = [head x| x<-st, x/=""]
          in (length y)==(length (unicAll y))

unicAll::[Char]->[Char]
unicAll (x:xs) | elem x xs = unicAll xs
               | otherwise = [x]++unicAll xs
unicAll [] = []

----------------------- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict 
buildFst [] = []
buildFst gr = let now = fst (gr!!0)
              in (evalFst gr now) ++ buildFst [(fst (gr!!n),snd (gr!!n))|n<-[0..(length gr - 1)], (fst (gr!!n))/=now]

evalFst :: Grammar -> Char -> Predict
evalFst gr now = [(now, sort (extandFst gr now [b|(a,b)<-gr, a==now]))]

extandFst :: Grammar->Char->[String]-> String
extandFst gr x ("":ys) = "$" ++ extandFst gr x ys
extandFst gr x (y:ys) | isUpper (head y) = let (_,b) = (evalFst gr (head y))!!0
                                           in if elem '$' b then b ++ extandFst gr x ([tail y] ++ ys)
                                           else b ++ extandFst gr x ys
                      | otherwise = [head y] ++ extandFst gr x ys
extandFst _ _ [] = []

----------------------- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict 
buildNxt = undefined

---------------------Тестові дані -----------------------------------
 
-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
--  LL(1)-граматики
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar

gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]

gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]

gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   



