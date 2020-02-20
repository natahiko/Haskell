{-# OPTIONS_GHC -Wall #-}
module Pr08Sparse where

import Data.Char
-- побудова простої монади синтаксичних аналізаторів 

newtype Sparse a = Sparse {parse:: String -> Maybe (a,String)}

item :: Sparse Char 
item = Sparse(\s -> case s of 
                     ""     -> Nothing
                     (c:cs) -> Just (c,cs))
--  parse item "c" .. parse item "nm" .. parse item ""   

-- Приклади аналізаторів
sat :: (Char -> Bool) -> Sparse Char
sat p = Sparse $ \s ->
  case s of
   ""     -> Nothing
   (c:cs) -> if p c then Just(c,cs) else Nothing 
--  parse (sat (=='g')) "gte" ..  parse (sat (=='g')) "te"  

char :: Char -> Sparse Char   
char c = sat (==c) 

digit :: Sparse Char
digit = sat isDigit  

oneOf :: [Char] -> Sparse Char
oneOf s = sat (\c -> elem c s)  -- sat (flip elem s)


-- Клас Functor і Applicative 
-- функтор
-- fmap :: (a -> b) -> Sparse a -> Sparse b    fmap == <$>   
instance Functor Sparse where
  fmap f (Sparse cs) = Sparse (\s -> case cs s of 
                                       Nothing     -> Nothing 
                                       Just (a,s1) -> Just(f a,s1)) 

intDigit :: Sparse Int
intDigit = digitToInt <$> digit

-- Клас Applicative 
-- аплікативний функтор         
-- pure :: a -> Sparse a 
-- (<*>) :: Sparse a -> Sparse (a->b) -> Sparse b  
instance Applicative Sparse where
  pure a = Sparse (\s -> Just(a,s))
  (Sparse cf) <*> (Sparse ca) = Sparse (\s -> 
      case cf s of 
        Nothing     -> Nothing 
        Just (f,s1) -> case ca s1 of 
                         Nothing     -> Nothing 
                         Just (a,s2) ->  Just (f a, s2)) 

string :: String -> Sparse String 
string "" = pure ""  
string (c:cs) = (:) <$> (char c) <*> (string cs) 

-- Клас  Monad 
-- return :: a -> Sparse a 
-- (>>=) :: Sparse a -> (a -> Sparse b) -> Sparse b    
instance Monad Sparse where
  return a = Sparse (\s -> Just(a,s))
  p >>= f = Sparse $ \s -> case parse p s of 
                             Nothing     -> Nothing 
                             Just (a,s1) -> let Sparse g = f a in g s1

-- Детермінований оператор вибору
(<|>) :: Sparse a -> Sparse a -> Sparse a
p <|> q = Sparse (\s -> case parse p s of
                           Nothing -> parse q s
                           res     -> res) 

many  :: Sparse a  -> Sparse [a]   --  нуль або багато
many p = some p <|> return []     

some  :: Sparse a  -> Sparse [a]   --  один або багато 						   
some p = do {a <- p; as <- many p; return (a:as)}

sign :: Sparse String 
sign = string "-" <|> return ""

number :: Sparse Int
number = do s <- sign 
            cs <- some digit
            return $ read (s ++ cs) 

-- Прості аналізатори 
spaces :: Sparse ()
spaces = many (sat isSpace) >> return ()

lexem :: Sparse a -> Sparse a
lexem p = do {a <- p; spaces ; return a}

reserved :: String -> Sparse ()
reserved s = do { _ <- string s; spaces} 

parens :: Sparse a -> Sparse a 
parens p = do reserved "(" 
              n <- lexem p 
              reserved ")" 
              return n              

--  Аналізатор послідовності              
chainl :: Sparse a -> Sparse (a -> a -> a) -> a -> Sparse a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Sparse a -> Sparse (a -> a -> a) -> Sparse a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a         

operator :: (Num a) => Sparse (a -> a -> a)
operator = inOperator <$> (oneOf "+-*")
  where inOperator :: (Num a) => Char -> (a -> a -> a)
        inOperator c = case c of {'+'-> (+); '-'->(-);'*'->(*); _ -> error "operator"} 


-- Виконання аналізатора		
runSparse :: Sparse a -> String -> Maybe a
runSparse m s =
  case parse m s of
    Just (res, []) -> Just res
    Just (_, _)    -> Nothing
    Nothing        -> Nothing

-- Мова виразів                  
-- number = ["-"] digit { digit }.
-- digit = "0" | "1" | ... | "8" | "9".
-- expr = term { addop term }.
-- term = factor { mulop factor }.
-- factor = "(" expr ")" | number.
-- addop = "+" | "-".
-- mulop = "*".

data Expr = Add Expr Expr | Mul Expr Expr | Sub Expr Expr | Lit Int
                     deriving Show

-- Аналізатор виразів
int :: Sparse Expr
int = do { n <-  lexem number; return (Lit n)}

infixOp :: String -> (a -> a -> a) -> Sparse (a -> a -> a)
infixOp x f = reserved x >> return f

addop, mulop :: Sparse (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)
mulop = infixOp "*" Mul

expr, term, factor :: Sparse Expr
expr   = term `chainl1` addop
term   = factor `chainl1` mulop
factor = int <|> parens expr

run :: String -> Maybe Expr
run st = runSparse (spaces >> expr) st    

-- Калькулятор виразів
eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n            

runEval :: String -> String 
runEval st = case run st of 
               Just ex -> show $ eval ex 
               Nothing -> "Sparse error"               

-- Repl (read-eval-print loop)			   
main :: IO()
main = loop 
 
loop :: IO()
loop = do
    putStr ">" 
    input <- getLine
    if null input then return () 
      else do putStrLn (runEval input)  
              loop
