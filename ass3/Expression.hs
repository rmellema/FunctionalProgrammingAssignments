module Expression(Expr, vars, evalExpr, toExpr) where
import Types
import Valuation
import Data.Char

data Expr =
     Val Integer
   | Var Name
   | Expr :+: Expr
   | Expr :-: Expr
   | Expr :*: Expr
   | Expr :/: Expr
   | Expr :%: Expr

instance Show Expr where
    show (Val n) = show n
    show (Var n) = n
    show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (e1 :-: e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (e1 :/: e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
    show (e1 :%: e2) = "(" ++ show e1 ++ " % " ++ show e2 ++ ")"

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge xs (y:ys)
    | x == y    = x : merge xs ys
    | otherwise = y : merge (x:xs) ys

vars :: Expr -> [Name]
vars (Val _) = []
vars (Var n) = [n]
vars (l :+: r) = vars l `merge` vars r
vars (l :-: r) = vars l `merge` vars r
vars (l :*: r) = vars l `merge` vars r
vars (l :/: r) = vars l `merge` vars r
vars (l :%: r) = vars l `merge` vars r

isComplete :: [Name] -> Valuation -> Bool
isComplete [] _  = True
isComplete xs [] = False
isComplete xs ys = and (map (/=Nothing) [lookup x ys | x <- xs])

evalExpr :: Expr -> Valuation -> Integer
evalExpr expr val
    | isComplete (vars expr) val = intEvalExpr expr val
    | otherwise                  = error "Incomplete evaluation given"

intEvalExpr :: Expr -> Valuation -> Integer
intEvalExpr (Val n) _      = n
intEvalExpr (Var n) v      = sure (lookup n v)
    where sure (Just b) = b
intEvalExpr (e1 :+: e2) v  = intEvalExpr e1 v + intEvalExpr e2 v
intEvalExpr (e1 :-: e2) v  = intEvalExpr e1 v - intEvalExpr e2 v
intEvalExpr (e1 :*: e2) v  = intEvalExpr e1 v * intEvalExpr e2 v
intEvalExpr (e1 :/: e2) v  = intEvalExpr e1 v `div` intEvalExpr e2 v
intEvalExpr (e1 :%: e2) v  = intEvalExpr e1 v `mod` intEvalExpr e2 v

-- Tokenizer for parser
tokenize :: String -> [String]
tokenize [] = []
tokenize (c:str)
    | isAlpha c = (c : takeWhile isAlpha str) : tokenize (dropWhile isAlpha str)
    | isDigit c = (c : takeWhile isDigit str) : tokenize (dropWhile isDigit str)
    | isOper  c = [c] : tokenize str
    | isSpace c = tokenize (dropWhile isSpace str)
    | isParen c = [c] : tokenize str
    | otherwise = error ("Unrecognised character: " ++ [c])

isOper :: Char -> Bool
isOper c = elem c "+-*/%"

isParen :: Char -> Bool
isParen c = isOpenParen c || isCloseParen c

isOpenParen :: Char -> Bool
isOpenParen c = elem c "("

isCloseParen :: Char -> Bool
isCloseParen c = elem c ")"

toExpr :: String -> Expr
toExpr s = parseE (tokenize s)

-- Finds a sub-expression, is very naive so doesn't support nested sub-expressions
findSubExpr :: [String] -> [String]
findSubExpr [] = []
findSubExpr (s:ss) = (fse 0 (s:ss))

fse :: Integer -> [String] -> [String]
fse i [] = []
fse i (f:fs)
    | isCloseParen (head f) && i < 1 = []
    | isCloseParen (head f)          = f : (fse (i-1) fs)
    | isOpenParen  (head f)          = f : (fse (i+1) fs)
    | otherwise                      = f : (fse i fs)

-- General parser
parseE :: [String] -> Expr
parseE [] = Val 0
parseE (e:es) = parseT (fst leftSub) (snd leftSub)
    where leftSub = parseF (e:es)

-- Parse + and -
parseE' :: Expr -> [String] -> Expr
parseE' expr (e:es) = expr

-- Parse * / % + -
parseT :: Expr -> [String] -> Expr
parseT expr [] = expr
parseT expr (e:es)
    | e == "*" = parseT ((expr) :*: fst rightSub) (snd rightSub)
    | e == "/" = parseT ((expr) :/: fst rightSub) (snd rightSub)
    | e == "%" = parseT ((expr) :%: fst rightSub) (snd rightSub)
    | e == "+" = parseT ((expr) :+: fst rightSub) (snd rightSub)
    | e == "-" = parseT ((expr) :-: fst rightSub) (snd rightSub)
    | otherwise = error ("Invalid operator: " ++ e)
    where rightSub = parseF es

-- Parse fact (but check if the fact is a subexpression)
-- Tuple returned is the parsed expression; remaining tokens to parse
parseF :: [String] -> (Expr, [String])
parseF (e:es)
    | isDigit (head e) = (Val (read e :: Integer), es)
    | isAlpha (head e) = (Var e, es)
    | e == "(" = (parseE subexpression, drop (1 + length subexpression) es)
    where subexpression = findSubExpr (es)

-- Simplify expressions
canSimplify :: Expr -> Bool
canSimplify (Val _) = False
canSimplify (Var _) = False
canSimplify e       = null $ vars e

simplifyExpr :: Expr -> Expr
simplifyExpr (Val n) = Val n
simplifyExpr (Var n) = Var n
simplifyExpr e@(e1 :+: e2)
    | canSimpe1 && canSimpe2 = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :+: e2
    | canSimpe2              = e1 :+: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :-: e2)
    | canSimpe1 && canSimpe2 = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :-: e2
    | canSimpe2              = e1 :-: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :*: e2)
    | canSimpe1 && canSimpe2 = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :*: e2
    | canSimpe2              = e1 :*: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :/: e2)
    | canSimpe1 && canSimpe2 = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :/: e2
    | canSimpe2              = e1 :/: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :%: e2)
    | canSimpe1 && canSimpe2 = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :%: e2
    | canSimpe2              = e1 :%: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
