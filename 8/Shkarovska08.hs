{-# OPTIONS_GHC -Wall #-}
module Shkarovska08 where

import Data.Char

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-------------------------------------------------- Задача 1 ------------------------------------
isNumbConst :: System -> Recur -> Bool 
isNumbConst _ Zero = True
isNumbConst syst (Super Zero st) = and [isNumbConst syst x | x<-st]
isNumbConst syst (Super Succ st) = and [isNumbConst syst x | x<-st]
isNumbConst _ (Super _ _) = False
isNumbConst syst (Name f) = isNumbConst syst (getSnd syst f)
isNumbConst _ _ = False

getSnd :: System -> String -> Recur
getSnd (s:st) f | (fst s)==f = snd s
                | otherwise = getSnd st f
getSnd [] _ = Zero

-------------------------------------------------- Задача 2 ------------------------------------
evRank :: System -> Recur -> Int 
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel a _) = a
evRank syst (Super b al) | al==[] = evRank syst b
                         | otherwise = evRank syst (head al)
evRank syst (Prim _ b) = (evRank syst b) - 1
evRank syst (Mini b _) = (evRank syst b) - 1
evRank syst (Name f) = evRank syst (getSnd syst f)

-------------------------------------------------- Задача 3 ------------------------------------
isNames :: System -> Bool 
isNames syst = let y = getNames syst
               in (y==(unic y)) && (and [dontUseMe a b| (a,b)<-syst])

getNames :: System -> [String]
getNames (s:st) = [fst s] ++ (getNames st)
getNames [] = []

unic :: [String] -> [String]
unic (x:xs) | elem x xs = unic xs
            | otherwise = [x] ++ (unic xs)
unic [] = []

dontUseMe :: String -> Recur -> Bool
dontUseMe _ Zero = True
dontUseMe _ Succ = True
dontUseMe _ (Sel _ _) = True
dontUseMe str (Super f st) = (dontUseMe str f) && (and [dontUseMe str x | x<-st])
dontUseMe str (Prim a b) = (dontUseMe str a) && (dontUseMe str b)
dontUseMe str (Mini a _) = (dontUseMe str a)
dontUseMe str (Name f) = str/=f
 

-------------------------------------------------- Задача 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur (s:st) f | f==(snd s) = True
                 | otherwise = isRecur st f
isRecur [] _ = False

-------------------------------------------------- Задача 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int
eval _ Zero _ = 0
eval _ Succ vl = vl!!0 + 1
eval syst (Super f ff) vl = eval syst f [eval syst x vl | x<-ff]
eval _ (Mini _ _) _ = 0
eval _ (Sel _ b) vl | b<0 || b>(length vl) = 0
                    | otherwise = vl!!(b-1)
eval syst (Name f) vl = eval syst (getSnd syst f) vl
eval syst (Prim f1 f2) vl = let nvl = [vl!!x|x<-[0..(length vl - 2)]]++[0]++[eval syst f1 vl]
                            in getRes syst f2 nvl (vl!!(length vl - 1))

getRes :: System -> Recur -> [Int] -> Int -> Int
getRes syst f vl t | (vl!!(length vl - 2))==t = vl!!(length vl - 1)
                   | otherwise = let y = [eval syst f vl]
                                     t2 = [(vl!!(length vl - 2))+1]
                                     nvl = [vl!!x | x<-[0..(length vl - 3)]] ++ t2 ++ y
                                 in getRes syst f nvl t

-------------------------------------------------- Задача 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart _ Zero _ = Just 0
evalPart _ Succ vl = Just (vl!!0 + 1)
evalPart syst (Super f ff) vl = let y = [evalPart syst x vl | x<-ff]
                                    bol = and [check x| x<-y]
                                in if bol then evalPart syst f (getMay y) else Nothing
evalPart _ (Sel _ b) vl | b<0 || b>(length vl) = Just 0
                        | otherwise = Just (vl!!(b-1))

evalPart syst (Name f) vl = evalPart syst (getSnd syst f) vl
evalPart syst (Prim f1 f2) vl = case evalPart syst f1 vl of
                    Just x -> let nvl = [(vl!!x)| x<-[0..(length vl - 2)]]++[0]++[x]
                              in getRes2 syst f2 nvl (vl!!(length vl - 1))
                    _ -> Nothing
evalPart syst (Mini f t) vl = getMiniRes syst f vl t 0

getMiniRes :: System -> Recur -> [Int] -> Int -> Int -> Maybe Int
getMiniRes syst f vl t n | t==n = Nothing
                         | otherwise = case (evalPart syst f (vl++[n])) of
                Just res -> if res==0 then Just n else (getMiniRes syst f vl t (n+1))
                _ -> Nothing

check :: Maybe Int -> Bool
check (Just _) = True
check Nothing = False
getMay :: [Maybe Int] -> [Int]
getMay ((Just x):xs) = [x] ++ (getMay xs)
getMay _ = []
getRes2 :: System -> Recur -> [Int] -> Int -> Maybe Int
getRes2 syst f vl t | (vl!!(length vl - 2))==t = Just (vl!!(length vl - 1))
                    | otherwise = let y = evalPart syst f vl
                                      t2 = [(vl!!(length vl - 2))+1]
                                  in case y of
                    Just x -> let nvl = [(vl!!x) | x<-[0..(length vl - 3)]] ++ t2 ++ [x]
                              in getRes2 syst f nvl t
                    _ -> Nothing

-------------------------------------------------- Задача 7 ------------------------------------
parseRec :: String -> Maybe System 
parseRec str = let nstr = filter (notSpace) str
               in case (splitBy nstr ';') of
    Just res -> let sys = [createSys x | x<-res]
                in case (hasNoth sys) of
            Just syst -> Just syst
            _ -> Nothing
    _ -> Nothing

createSys :: String -> Maybe (String, Recur)
createSys str | not (elem '=' str) = Nothing
              | otherwise = case (splitBy str '=') of
                Just y -> let name = y!!0
                          in if (length y)>2 then Nothing else case (getRecur (y!!1)) of
                    Just recur -> Just (name, recur)
                    _ -> Nothing
                _ -> Nothing



getRecur :: String -> Maybe Recur
getRecur "z1" = Just Zero
getRecur "a1" = Just Succ
getRecur ('s':(a:(b:[]))) = case (getInt [a]) of
           Just a1 -> case (getInt [b]) of
                 Just b1 -> Just (Sel a1 b1)
                 _ -> Nothing
           _ -> Nothing
getRecur('(':xs) | xs!!(length xs - 1)/=')' = Nothing
                 | not (elem ':' xs) = Nothing
                 | otherwise = let nxs = [xs!!x| x<-[0..(length xs - 2)]]
                               in case (getToCh (Just ("",nxs)) ':') of
            Just res -> case (getRecur (fst res)) of
                    Just f1 -> case (splitBy2 (snd res) ',') of
                      Just splited -> case (getRecArr splited) of
                          Just f2 -> Just (Super f1 f2)
                          _ -> Nothing
                      _ -> Nothing
                    _ -> Nothing
            _ -> Nothing
getRecur ('{':xs) | xs!!(length xs - 1)/='}' = Nothing
                  | not (elem ',' xs) = Nothing
                  | otherwise = let nxs = [xs!!x| x<-[0..(length xs - 2)]]
                                in case (splitBy nxs ',') of
            Just res -> case (getRecur (res!!0)) of
                    Just f1 -> case (getInt (res!!1)) of
                        Just tt -> Just (Mini f1 tt)
                        _ -> Nothing
                    _ -> Nothing
            _ -> Nothing

getRecur ('[':xs) | xs!!(length xs - 1)/=']' = Nothing
                  | not (elem ',' xs) = Nothing
                  | otherwise = let nxs = [xs!!x| x<-[0..(length xs - 2)]]
                                in case (splitBy nxs ',') of
            Just res -> case (getRecur (res!!0)) of
                    Just f1 -> case (getRecur (res!!0)) of
                        Just f2 -> Just (Prim f1 f2)
                        _ -> Nothing
                    _ -> Nothing
            _ -> Nothing
getRecur str = Just (Name str)

getRecArr :: [String] -> Maybe [Recur]
getRecArr xs = let y = [getRecur x | x<- xs]
               in case (allCorrect y) of
        Just res -> Just res
        _ -> Nothing

allCorrect :: [Maybe Recur] -> Maybe [Recur]
allCorrect ((Just r):xs) = case (allCorrect xs) of
         Just res -> Just ([r]++res)
         _ -> Nothing
allCorrect (Nothing:_) = Nothing
allCorrect [] = Just [] 

getInt :: String -> Maybe Int
getInt str | notDigit str = Nothing
           | otherwise = Just (read str :: Int)
notDigit :: String -> Bool
notDigit (x:xs) | isDigit x = notDigit xs
                | otherwise = True
notDigit [] = False

notSpace :: Char -> Bool
notSpace ch = not (isSpace ch)

splitBy :: String -> Char -> Maybe [String]
splitBy "" _ = Just []
splitBy str ch = case (getToCh (Just ("",str)) ch) of
        Just r -> let y = splitBy (snd r) ch
                  in case y of
            Just y2 -> Just ([fst r]++y2)
            _ -> Nothing
        _ -> Nothing

splitBy2 :: String -> Char -> Maybe [String]
splitBy2 "" _ = Just []
splitBy2 ('(':xs) ch = case (getToCh (Just ("",xs)) ')') of
        Just r -> let y = splitBy2 (snd r) ch
                  in case y of
            Just y2 -> Just ([(fst r)++")"]++y2)
            _ -> Nothing
        _ -> Nothing
splitBy2 str ch = case (getToCh (Just ("",str)) ch) of
        Just r -> let y = splitBy2 (snd r) ch
                  in case y of
            Just y2 -> Just ([fst r]++y2)
            _ -> Nothing
        _ -> Nothing

getToCh :: Maybe (String,String) -> Char -> Maybe (String,String)
getToCh (Just (res, "")) _ = Just (res, "") 
getToCh (Just (res, str)) ch | (head str)==ch = Just (res, tail str)
                             | otherwise = getToCh (Just (res++[head str], tail str)) ch
getToCh Nothing _ = Nothing

hasNoth :: [Maybe (String, Recur)] -> Maybe [(String, Recur)]
hasNoth ((Just x):st) = case (hasNoth st) of 
           Just res -> Just ([x]++res)
           _ -> Nothing
hasNoth (Nothing:_) = Nothing
hasNoth [] = Just []

--------------------- ������ ���� -  -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2,sysStr3 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	      \  notSignum = [(a1:z1),(z1:s21)];\n\
		  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
		  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
sysStr3 = "subtractionAbs3=(subtractionAbs:s31,(addition:s32,s33))"