module Expression(Expr, vars, evalExpr) where
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

leftHand :: Expr -> Expr
leftHand (l :+: _) = l
leftHand (l :-: _) = l
leftHand (l :*: _) = l
leftHand (l :/: _) = l
leftHand (l :%: _) = l
leftHand otherwise = error "No left hand side!"

rightHand :: Expr -> Expr
rightHand (_ :+: r) = r
rightHand (_ :-: r) = r
rightHand (_ :*: r) = r
rightHand (_ :/: r) = r
rightHand (_ :%: r) = r
rightHand otherwise = error "No right hand side!"

vars :: Expr -> [Name]
vars (Val _) = []
vars (Var n) = [n]
vars expr    = vars (leftHand expr) `merge` vars (rightHand expr)

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

--tokenize :: String -> [String]
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
isParen c = elem c "()"

toExpr :: String -> Expr
toExpr = Var

-- Finds a sub-expression, is very naive so doesn't support nested sub-expressions
findSubExpr :: [String] -> [String]
findSubExpr [] = []
findSubExpr [s] = [s]
findSubExpr (s:ss) = takeWhile ( /= ")" ) (s:ss)

-- General parser
parseE :: [String] -> Expr
parseE [] = Val 0
parseE [e] = parseF [e]
parseE (e:es)
    | e == "(" = parseT (parseE leftSub) (drop (1 + length leftSub ) es)
    where leftSub = findSubExpr es

-- Parse atoms (but check if the atom is a subexpression)
parseF :: [String] -> Expr
parseF [e] -- Single element, so a variable or a literal
    | isDigit (head e) = Val (read e)
    | isAlpha (head e) = Var e
    | otherwise        = error ("Malformed expression: " ++ e)
parseF (e:es) = parseE (e:es) -- Lists are sub expressions which can be parsed with the general expression parser

parseT :: Expr -> [String] -> Expr
-- From here on needs updating
parseT expr (e:es)
    | e == "("         = (parseT' (parseF e') (drop (length e' +1) es))
    | isDigit (head e) = Val (read e :: Integer)
    | otherwise        = Var e
    where e' = dropWhile (/= ")") es

parseT' :: Expr -> [String] -> Expr
parseT' expr (e:es)
    | e == "*" = expr :*: parseT' expr es
    | otherwise = expr
