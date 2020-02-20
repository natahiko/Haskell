{-# OPTIONS_GHC -Wall #-}
module Shkarovska10 where

import Data.List
--import qualified Text.ParserCombinators.Parsec as P

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

--(sort .nub. states) nda5
--(sort .nub. states) da5
states :: Automation -> [State]
states (a,b,((x,_,_):xs)) = sort ([x] ++ (states (a,b,xs)))
states (_,b,[]) = b

isDeter :: Automation -> Bool
isDeter (_,_,tr) = (null .(filter (\(_,_,l)-> l==Eps))) tr &&
                   (null .(filter ((>1).length)) . group . sort . (map(\(s,_,l)-> (s,l)))) tr

-- Задача 1 --------------------------------------------------------------------------------------
simplify :: RE -> RE
simplify (Seq r1 r2) = Seq (simplify r1) (simplify r2)
simplify (Alt r1 r2) = Alt (simplify r1) (simplify r2)
simplify (Plus re) = let nre = simplify re
                     in Seq nre (Rep nre)
simplify (Opt re) = Alt (simplify re) Null
simplify (Rep re) = Rep (simplify re)
simplify x = x

-- Задача 2 --------------------------------------------------------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, ter,_) a = elem a ter

isEssential :: Automation -> State -> Bool 
isEssential (_,ter, arr) a | elem a ter = True
                           | a>(length arr) = False
                           | otherwise = case arr!!(a-1) of
            (_,_, C _) -> True
            _ -> False

-- Задача 3 --------------------------------------------------------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, ter, arr) a | elem a ter = []
                                | otherwise = getAll arr a

getAll :: [Transition] -> State -> [Transition]
getAll (x@(b,_,_):xs) a | b==a =  [x] ++ (getAll xs a)
                        | otherwise = getAll xs a
getAll [] _ = []

-- Задача 4 --------------------------------------------------------------------------------------
labels :: [Transition] -> [Label]
labels trs = let al = [b | (_,_,b)<-trs,b/=Eps]
             in nub al

-- Задача 5 --------------------------------------------------------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA da@(st,_,_) str = isAccDa st da str

isAccDa :: State -> Automation -> String -> Bool
isAccDa st (_,ter,_) "" = if (elem st ter) then True else False
isAccDa st da@(_, ter, arr) str | elem st ter = False
                                | otherwise = let next = getAll arr st
                                                  suit = [n| (_,n,C b)<-next, b==(head str)]
                                              in if (length suit < 1) then False 
                                                 else isAccDa (suit!!0) da (tail str)

-- Задача 6 --------------------------------------------------------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]
closure2 :: Automation -> [State] -> [State]

stStep (_,_,arr) st n = [b | (a,b,c)<-arr, c==n && a==st]
setStep da sts n = reverse (concat [stStep da x n | x<-sts])
closure da sts = let n = nub (setStep da sts Eps)
                    -- next = nub (sts ++ n)
                 in if n==sts then (sort n) else closure2 da n

closure2 da sts = let n = setStep da sts Eps
                      next = nub (sts ++ n)
                  in if next==sts then (sort next) else closure2 da next

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts da@(st,_,_) str = isAcc st da str

isAcc :: State -> Automation -> String -> Bool
isAcc st (_,ter,_) "" = if elem st ter then True else False
isAcc st da@(_,ter,_) str | elem st ter = False
                          | otherwise = let n1 = stStep da st (C (head str))
                                            n2 = setStep da (closure da  [st]) (C (head str))
                                            n = nub (concat [n1,n2])
                                        in (or [isAcc x da (tail str) | x<-n])

-- Задача 8 --------------------------------------------------------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int)
make (Term a) beg fin nxt = let st = (beg, fin, C a)
                            in ([st], nxt)
make (Rep r1) beg fin nxt = let r2 = make r1 nxt (nxt + 1) (nxt+2)
                                st1 = (beg, fin, Eps)
                                st2 = (beg, nxt, Eps)
                                st3 = (nxt+1, nxt, Eps)
                                st4 = (nxt+1, fin, Eps)
                                resFirst = [st1, st2] ++ (fst r2) ++ [st3, st4]
                            in (resFirst, (snd r2))
make (Seq r1 r2) beg fin nxt = let r11 = make r1 beg nxt (nxt+2)
                                   r21 = make r2 (nxt+1) fin (snd r11)
                                   st1 = (nxt, nxt+1, Eps)
                               in ((fst r11) ++ [st1] ++ (fst r21), (snd r21))
make (Alt r1 r2) beg fin nxt = let r11 = make r1 nxt (nxt+1) (nxt+4)
                                   r21 = make r2 (nxt+2) (nxt+3) (snd r11)
                                   st1 = (beg, nxt, Eps)
                                   st2 = (nxt+1, fin, Eps)
                                   st3 = (beg, nxt+2, Eps)
                                   st4 = (nxt+3, fin, Eps)
                               in ([st1]++(fst r11)++[st2, st3]++(fst r21)++[st4], snd r21)
make _ beg fin nxt = let st = (beg, fin, Eps)
                     in ([st],  nxt)

-- Задача 9 --------------------------------------------------------------------------------------
parseReg :: String -> Maybe RE 
parseReg = undefined

-- Задача 10 --------------------------------------------------------------------------------------
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined  

makeDA :: Automation -> Automation
makeDA  = undefined

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )