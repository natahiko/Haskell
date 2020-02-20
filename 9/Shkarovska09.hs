{-# OPTIONS_GHC -Wall #-}
module Shkarovska09 where

-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 --------------------------------------------------------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b (x:xs) | fst x == a = [(a,b)]++xs
                       | otherwise = [x] ++ (updateValue a b xs)
updateValue a b [] = [(a,b)]

-- Задача 2 --------------------------------------------------------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A (x:xs)) (I a) (I b) | fst x == a = A ([(a,b)] ++ xs)
                                   | otherwise = let (A res) = (updateArray (A xs) (I a) (I b))
                                                 in A ([x] ++ res)
updateArray (A []) (I a) (I b) = A [(a,b)]
updateArray _ _ _ = A []

-- Задача 3 --------------------------------------------------------------------------------------
applyOp :: Op -> Value -> Value -> Value 
applyOp Index (A (x:xs)) (I a) | fst x == a = I (snd x)
                               | otherwise = applyOp Index (A xs) (I a)
applyOp Add (I a) (I b) = I (a+b)
applyOp Minus (I a) (I b) = I (a-b)
applyOp Mul (I a) (I b) = I (a*b)
applyOp Less (I a) (I b) | a<b = I 1
                         | otherwise = I 0
applyOp Equal (I a) (I b) | a==b = I 1
                          | otherwise = I 0
applyOp _ _ _ = I 0

-- Задача 4 --------------------------------------------------------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const c) _ _ = I c
evExp (Var e) dfx (s:st) | fst s == e = snd s
                         | otherwise = evExp (Var e) dfx st
evExp (Var _) _ [] = I 0
evExp (OpApp f a b) dfx st = let a1 = evExp a dfx st
                                 b1 = evExp b dfx st
                             in applyOp f a1 b1
evExp (Cond c a b) dfx st = let (I res) = evExp c dfx st
                            in if res/=0 then evExp a dfx st
                                         else evExp b dfx st
evExp (FunApp i es) dfx st = let (ass, ef) = [snd x | x<-dfx, fst x == i]!!0
                                 vs = evArgs es dfx st
                                 new = [(getName (ass!!x), vs!!x)| x<-[0..(length vs - 1)]]
                             in evExp ef dfx new
getName :: VarDef -> Id
getName (Int a) = a
getName (Arr a) = a

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]  
evArgs ex dfx st = [evExp x dfx st | x<-ex]

-- Задача 5 --------------------------------------------------------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign i ex) dfx _ st = addRewrState st i (evExp ex dfx st)
evStmt (AssignA i exp1 exp2) dfx _ st = let res1 = evExp exp1 dfx st
                                            res2 = evExp exp2 dfx st
                                            arr = ([b| (a,b)<-st, a==i]!!0)
                                            narr = updateArray arr res1 res2
                                        in addRewrState st i narr
evStmt (If ex st1 st2) dfx dpx st = let nex = evExp ex dfx st
                                    in if isNull nex then (evStmt st2 dfx dpx st) 
                                       else (evStmt st1 dfx dpx st)
evStmt (While ex stmt) dfx dpx st = until (isNotEnd ex dfx) (evStmt stmt dfx dpx) st
evStmt (Call i exps) dfx dpx st = let (ass, st1) = [b|(a,b)<-dpx, a==i]!!0
                                      vals = [evExp x dfx st| x<-exps]
                                      names = [getName x | x<-ass]
                                      res = updateSts [(names!!x, vals!!x)| x<-[0..(length vals - 1)]] st
                                   in evStmt st1 dfx dpx res
evStmt (Block _ _) _ _ st = st --let nst = addAllSt in st

updateSts :: StateP -> StateP -> StateP
updateSts xs (y:ys) | isEl (fst y) xs = updateSts xs ys
                    | otherwise = [y] ++ (updateSts xs ys)
updateSts xs [] = xs

isEl :: Id -> StateP -> Bool
isEl e (v:vs) | fst v == e = True
              | otherwise = isEl e vs
isEl _ [] = False

isNotEnd :: Exp -> [FunDef] -> StateP -> Bool
isNotEnd exp1 fd st = let y = evExp exp1 fd st 
                      in y==(I 0)


addRewrState :: StateP -> Id -> Value -> StateP
addRewrState (x:xs) i val | (fst x) == i = [(i, val)]++xs
                          | otherwise = (addRewrState xs i val)
addRewrState [] i val = [(i, val)]

isNull :: Value -> Bool
isNull (I a) = a==0
isNull (A xs) = xs==[]

-- Задача 6 --------------------------------------------------------------------------------------
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It
iswfExp (Var e) ve _ | isElem e ve = Just ([b|(a,b)<-ve, a==e]!!0)
                     | otherwise = Nothing
iswfExp (OpApp op a b) ve fe = case (iswfExp a ve fe) of
         Just a1 -> case (iswfExp b ve fe) of
              Just b1 -> iswfOp op [a1, b1]
              _ -> Nothing
         _ -> Nothing
iswfExp (Cond a b c) ve fe = case (iswfExp a ve fe) of
    Just a1 -> case (iswfExp b ve fe) of
        Just b1 -> case (iswfExp c ve fe) of
            Just c1 -> iswfCond [a1, b1, c1]
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
iswfExp (FunApp i es) ve fe | isntIn i fe = Nothing
                            | otherwise = let nes = [iswfExp x ve fe| x<-es]
                                              nfe = [b| (a,b)<-fe, a==i]
                                           in if (isEqual nes (nfe!!0)) then Just It else Nothing

isEqual :: [Maybe Type] -> [Type] -> Bool
isEqual ((Just x):xs) (y:ys) | x==y = isEqual xs ys
                             | otherwise = False
isEqual [] [] = True
isEqual _ _ = False

isntIn :: Id -> FunEnv -> Bool
isntIn i (x:xs) | fst x == i = False
                | otherwise = isntIn i xs
isntIn _ [] = True

isElem :: Id -> VarEnv -> Bool
isElem e (v:vs) | fst v == e = True
                | otherwise = isElem e vs
isElem _ [] = False

-- Задача 7 --------------------------------------------------------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign i ex) ve fe _ = let y = [b|(a,b)<-ve, a==i]
                                 in if (length y < 1) then False else
                                       case (iswfExp ex ve fe) of
            Just type1 -> type1==(y!!0)
            _ -> False
iswfStmt (AssignA i exp1 exp2) ve fe _ = case (iswfExp exp1 ve fe) of
        Just type1 -> case (iswfExp exp2 ve fe) of
                Just type2 -> if (length [b|(a,b)<-ve, a==i] < 1) then False else iswfAssignA [([b|(a,b)<-ve, a==i]!!0), type1, type2]
                _ -> False
        _ -> False
iswfStmt (If ex st1 st2) ve fe pe = case (iswfExp ex ve fe) of
        Just It -> (iswfStmt st1 ve fe pe) && (iswfStmt st2 ve fe pe)
        _ -> False
iswfStmt (While ex st) ve fe pe = case (iswfExp ex ve fe) of
        Just It -> iswfStmt st ve fe pe
        _ -> False
iswfStmt (Call i exps) ve fe pe = let y = [b| (a,b)<-pe, a==i]
                                      es = [iswfExp e ve fe | e<-exps]
                                      bol = (length y < 1)
                                  in if bol then False else (isEqual es (y!!0))
iswfStmt (Block vars sts) ve fe pe = let nvars = getArray vars
                                         nve = updateVars nvars ve
                                     in and [iswfStmt st nve fe pe | st<-sts]

updateVars :: VarEnv -> VarEnv -> VarEnv
updateVars xs (y:ys) | isElem (fst y) xs = updateVars xs ys
                     | otherwise = [y] ++ (updateVars xs ys)
updateVars xs [] = xs

getArray :: [VarDef] -> VarEnv
getArray ((Arr i):xs) = [(i,At)] ++ (getArray xs)
getArray ((Int i):xs) = [(i,It)] ++ (getArray xs)
getArray [] = []

-- Задача 8 --------------------------------------------------------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (i, val) fe = let (vd, ex) = val 
                             ve = getArray vd
                             vars = [b| (a,b)<-fe, a==i]!!0
                             bol = isElem2 i fe
                         in case (iswfExp ex ve fe) of
        Just It -> bol && (isCorElem vars ve)
        _ -> False

isCorElem :: [Type] -> [(Id,Type)] -> Bool
isCorElem (x:xs) ((_,y):ys) | x==y = True
                            | otherwise = isCorElem xs ys
isCorElem [] [] = True
isCorElem _ _ = False

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (_, val) ve dfx dpx = let (vd, st) = val
                                      arr = getArray vd
                                      nve = updateVars arr ve
                                  in iswfStmt st nve dfx dpx
isElem2 :: Id -> FunEnv -> Bool
isElem2 e (v:vs) | fst v == e = True
                 | otherwise = isElem2 e vs
isElem2 _ [] = False

-- Задача 9 --------------------------------------------------------------------------------------
iswfProgram :: Program -> Bool
iswfProgram = undefined 


--- Допоміжні функції ----------------------------------------------------------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
