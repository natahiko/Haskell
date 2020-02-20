{-# OPTIONS_GHC -Wall #-}
module Shkarovska07 where

import Data.List

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-дерево ������� t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- ������� �������������� B-дерево (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

hd :: BinTreeM a -> (Int, Int)
hd EmptyM = (0,0)
hd (NodeM _ _ l r) = let (hl, dl) = hd l
                         (hr, dr) = hd r
                     in ((max hl hr)+1, maximum [dl,dr,abs(dl-dr)])
h,d ::BinTreeM a -> Int
--висота дерева
--h NodeM a b tr1 tr2 = 1 + max (h tr1) (h tr2)
--h EmptyM = 0
h = fst . hd
--розбалансованість
d = snd . hd

---------------------------------------------------- Задача 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch (NodeM _ _ EmptyM EmptyM) = False
isSearch (NodeM a _ EmptyM (NodeM ar _ EmptyM EmptyM)) = a<ar
isSearch (NodeM a _ EmptyM tr2@(NodeM ar _ _ _)) = a<ar && (isSearch tr2) && (allMore a (allKeysM tr2))
isSearch (NodeM a _ (NodeM al _ EmptyM EmptyM) EmptyM) = al<a
isSearch (NodeM a _ tr1@(NodeM al _ _ _) EmptyM) = al<a && (isSearch tr1) && (allLess a (allKeysM tr1))
isSearch (NodeM a _ (NodeM al _ EmptyM EmptyM) (NodeM ar _ EmptyM EmptyM)) = al<a && a<ar
isSearch (NodeM a _ (NodeM al _ EmptyM EmptyM) tr2@(NodeM ar _ _ _)) 
                                             = al<a && a<ar && (isSearch tr2) && (allMore a (allKeysM tr2))
isSearch (NodeM a _ tr1@(NodeM al _ _ _) (NodeM ar _ EmptyM EmptyM)) 
                                             = al<a && a<ar && (isSearch tr1) && (allLess a (allKeysM tr1))
isSearch (NodeM a _ tr1@(NodeM al _ _ _) tr2@(NodeM ar _ _ _)) 
                                             = al<a && a<ar && (isSearch tr1) && (isSearch tr2)
                                               && (allMore a (allKeysM tr2)) && (allLess a (allKeysM tr1))

allLess :: (Ord a) => a -> [a] -> Bool
allLess a (x:xs) | a>x = allLess a xs
                 | otherwise = False
allLess _ [] = True			
allMore :: (Ord a) => a -> [a] -> Bool
allMore a (x:xs) | a<x = allMore a xs
                 | otherwise = False
allMore _ [] = True			 

allKeysM :: (Ord a) => BinTreeM a -> [a]
allKeysM EmptyM  = []
allKeysM (NodeM a _ EmptyM EmptyM) = [a]
allKeysM (NodeM a _ tr1 EmptyM) = [a] ++ (allKeysM tr1)
allKeysM (NodeM a _ EmptyM tr2) = [a] ++ (allKeysM tr2)
allKeysM (NodeM a _ tr1 tr2) = [a] ++ (allKeysM tr1) ++ (allKeysM tr2)

---------------------------------------------------- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM a _ tr1 tr2) v | v==a = True
                                 | otherwise = (elemSearch tr1 v) || (elemSearch tr2 v)

---------------------------------------------------- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM v = NodeM v 1 EmptyM EmptyM
insSearch (NodeM a b EmptyM EmptyM) v | a>v = NodeM a b (NodeM v 1 EmptyM EmptyM) EmptyM
                                      | a<v = NodeM a b EmptyM (NodeM v 1 EmptyM EmptyM)
                                      | otherwise = NodeM a (b+1) EmptyM EmptyM
insSearch (NodeM a b tr1 EmptyM) v | a>v = NodeM a b (insSearch tr1 v) EmptyM
                                   | a<v = NodeM a b tr1 (NodeM v 1 EmptyM EmptyM)
                                   | otherwise = NodeM a (b+1) tr1 EmptyM
insSearch (NodeM a b EmptyM tr2) v | a>v = NodeM a b (NodeM v 1 EmptyM EmptyM) tr2
                                   | a<v = NodeM a b EmptyM (insSearch tr2 v)
                                   | otherwise = NodeM a (b+1) EmptyM tr2
insSearch (NodeM a b tr1 tr2) v | a>v = NodeM a b (insSearch tr1 v) tr2
                                | a<v = NodeM a b tr1 (insSearch tr2 v)
                                | otherwise = NodeM a (b+1) tr1 tr2

---------------------------------------------------- Задача 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch (NodeM a b EmptyM EmptyM) v | a==v && b>1 = NodeM a (b-1) EmptyM EmptyM
                                      | a==v = EmptyM  
                                      | otherwise = (NodeM a b EmptyM EmptyM)
delSearch (NodeM a b tr1 EmptyM) v | a==v && b>1 = NodeM a (b-1) tr1 EmptyM
                                   | a==v = tr1
                                   | a>v = NodeM a b (delSearch tr1 v) EmptyM
                                   | otherwise = (NodeM a b tr1 EmptyM)
delSearch (NodeM a b EmptyM tr2) v | a==v && b>1 = NodeM a (b-1) EmptyM tr2
                                   | a==v = tr2
                                   | a<v = NodeM a b EmptyM (delSearch tr2 v)
                                   | otherwise = (NodeM a b EmptyM tr2)
delSearch (NodeM a b tr1 tr2) v | a==v && b>1 = NodeM a (b-1) tr1 tr2
                                | a==v = let (a1,b1) = findNext tr2
                                             ntr1 = delSearch tr1 a1
                                             ntr2 = delSearch tr2 a1
                                         in NodeM a1 b1 ntr1 ntr2
                                | a>v = NodeM a b (delSearch tr1 v) tr2
                                | a<v = NodeM a b tr1 (delSearch tr2 v)

findNext :: (Ord a) => BinTreeM a -> (a, Int)
findNext tr@(NodeM a b EmptyM _) = (a,b)
findNext (NodeM a b tr1 _) = findNext tr1


---------------------------------------------------- Задача 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList [] = []
sortList l = let tr = createTree EmptyM l --try to use until 
             in getLeftAndDel tr

getLeftAndDel :: (Ord a) => BinTreeM a -> [a] 
getLeftAndDel (NodeM a b EmptyM EmptyM) = [a|_<-[1..b]] -- rewrite without X
getLeftAndDel (NodeM a b tr1 EmptyM) = (getLeftAndDel tr1) ++ [a|_<-[1..b]]
getLeftAndDel (NodeM a b EmptyM tr2) = [a|_<-[1..b]] ++ (getLeftAndDel tr2)
getLeftAndDel (NodeM a b tr1 tr2) = (getLeftAndDel tr1) ++ [a|_<-[1..b]] ++ (getLeftAndDel tr2)

notEmpty :: (Ord a) => BinTreeM a -> Bool
notEmpty (NodeM _ _ _ _) = True
notEmpty EmptyM = False

createTree :: (Ord a) => BinTreeM a -> [a] -> BinTreeM a
createTree tr (x:xs) = createTree (insSearch tr x) xs
createTree tr [] = tr

---------------------------------------------------- Задача 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a -> BInform a
findBInform tr = BInform {hB=findHeight tr, minB=findMin tr, maxB=findMax tr} --uncorect print

findMin :: (Bounded a, Ord a) => Btree a -> a
findMin (NodeB a []) = head a
findMin (NodeB _ trs) = findMin (head trs)

findMax :: (Bounded a, Ord a) => Btree a -> a
findMax (NodeB a []) = a!!(length a - 1)
findMax (NodeB _ trs) = findMax (trs!!(length trs - 1))

findHeight :: (Bounded a, Ord a) => Btree a -> Int
findHeight (NodeB _ []) = 0
findHeight (NodeB _ trs) = 1 + maximum [findHeight tr| tr<-trs] 

---------------------------------------------------- Задача 7 ------------------------------------
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree n tr@(NodeB a trs) | notCorHeight trs = False
                           | (length trs)/=(length a + 1) = False
                           | otherwise = (allKeysSorted tr) && (and [allKeysLong n trx | trx<-trs]) && 
                             (length a)>=1 && (length a)<=(2*n-1)

notCorHeight :: (Bounded a, Ord a) => [Btree a] -> Bool 
notCorHeight [] = True
notCorHeight (tr:trs) = let h = (findHeight tr)
                        in (or [h/=(findHeight x)| x<-trs])

allKeysSorted :: (Bounded a, Ord a) => Btree a -> Bool 
allKeysSorted (NodeB a []) = isSorted a 
allKeysSorted (NodeB a trs) = isSorted a && and [allKeysSorted y | y<-trs] 
                              && and [allKeysLess (a!!y) (trs!!y) |y<-[0..(length a -1)]]
                              && and [allKeysMore (a!!y) (trs!!(y+1)) |y<-[0..(length a -1)]]

allKeysLess :: (Bounded a, Ord a) => a -> Btree a -> Bool 
allKeysLess _ (NodeB _ []) = True
allKeysLess b (NodeB a _) = and [x<b|x<-a]
allKeysMore :: (Bounded a, Ord a) => a -> Btree a -> Bool 
allKeysMore _ (NodeB _ []) = True
allKeysMore b (NodeB a _) = and [x>b|x<-a]

isSorted :: (Bounded a, Ord a) => [a] -> Bool 
isSorted (x:(y:_)) = x<y
isSorted _ = True

allKeysLong :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
allKeysLong n (NodeB a []) = (length a)>=(n-1) && (length a)<=(2*n-1)
allKeysLong n (NodeB a trs) = (length a)>=(n-1) && (length a)<=(2*n-1) 
                               && (and [allKeysLong n x | x<-trs])

---------------------------------------------------- Задача 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ tr1 tr2 = let y1 = sort (getKeyArray tr1)
                        y2 = sort (getKeyArray tr2)
                    in y1==y2
                    -- not (isBtree n tr1 && isBtree n tr2) = False
                    -- otherwise 
getKeyArray :: Btree a -> [a]
getKeyArray (NodeB a trs) = a ++ (concat [(getKeyArray (trs!!x)) | x<-[0..(length trs - 1)]])

---------------------------------------------------- Задача 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr a = elem a (getKeyArray tr)

---------------------------------------------------- Задача 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a ->Btree a
insBtree t tr@(NodeB _ trs) v = let (NodeB na ntrs) = if isFull t tr
                                                      then rewrite t tr else tr
                                    nextcode = if v<(head na) then 0 else if v>(na!!(length na - 1)) then (length ntrs - 1) 
                                       else [x|x<-[0..(length na - 1)],v<(na!!x)]!!0
                                    next = if ntrs==[] then NodeB [] [] 
                                           else ntrs!!nextcode
                                    anext = if (isFull t next) then (insert2 (getCenter t next) na) else na
                                in if trs==[] then NodeB (insert2 v na) ntrs else (NodeB anext ([ntrs!!x|x<-[0..(nextcode - 1)]] ++
                                   [insBtree t next v] ++ [ntrs!!x|x<-[(nextcode+1)..(length ntrs - 1)]]))

rewrite :: Ord a => Int -> Btree a -> Btree a 
rewrite _ (NodeB [] []) = NodeB [] []
rewrite t (NodeB a []) = (NodeB [a!!(t-1)] [NodeB [a!!x|x<-[0..t-2]] [],
                              NodeB [a!!x|x<-[t..(length a - 1)]] []])
rewrite t (NodeB a trs) = (NodeB [a!!(t-1)] [NodeB [a!!x|x<-[0..t-2]] [trs!!x|x<-[0..(t-1)]],
                              NodeB [a!!x|x<-[t..(length a - 1)]] [trs!!x|x<-[t..(length trs - 1)]]])

getCenter :: Ord a => Int -> Btree a -> a
getCenter t (NodeB a _) = a!!(t-1)

insert2 :: Ord a => a -> [a] -> [a]
insert2 v (x:xs) | v>x = [x] ++ (insert2 v xs)
                | v==x = [x,x] ++ xs
                | otherwise = [v,x]++xs
insert2 v [] = [v]

isFull :: Ord a => Int -> Btree a -> Bool
isFull n (NodeB a _) = (length a)==(2*n-1)

insertKey :: Ord a => a -> [a] -> [a]
insertKey a (x:xs)| a<x = [x] ++ (insertKey a xs)
                  | a==x = [a,a] ++ xs
                  | otherwise = [a,x]++xs
insertKey a [] = [a]


--------------------------------------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   
{-
tiB1 :: Btree Char 
tiB1 = NodeB ['G','M','P','X'] 
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]

tBtr1 :: Btree Int
tBtr1 = NodeB [5,10,12] [ts0,ts1,ts2,ts3]
   where ts0 = NodeB [1,3  ] []   --- ,4,5] []  --
         ts1 = NodeB [6,6 ,8,9,10] [] --- ,8,9,10] []  -- ] []   
         ts2 = NodeB [11,11,12,12] []
         ts3 = NodeB [16,16] [] -- ,18,19,20] [] 

tBtr2 :: Btree Int 
tBtr2 = NodeB [15] [ts10,ts11]
  where ts10 = NodeB [11,13] [] 
        ts11 = NodeB [21,22] []  
-}
tBt1, tBt2, tBt5, tBt6, tBt7, tBt8, tBt9 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]