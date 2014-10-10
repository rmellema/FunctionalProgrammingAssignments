module Expression(Expr, vars, evalExpr, toExpr, module Types, Valuation) where
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
    show (e1 :+: e2) = intShow e1 e2 " + "
    show (e1 :-: e2) = intShow e1 e2 " - "
    show (e1 :*: e2) = intShow e1 e2 " * "
    show (e1 :/: e2) = intShow e1 e2 " / "
    show (e1 :%: e2) = intShow e1 e2 " % "

-- Internal show function to cut down on lines in show inheritance
intShow :: Expr -> Expr -> String -> String
intShow (Val l) (Val r) op = show l ++ op ++ show r
intShow (Val l) (Var r) op = show l ++ op ++ show r
intShow (Val l) (    r) op = show l ++ op ++ "(" ++ show r ++ ")"
intShow (Var l) (Val r) op = show l ++ op ++ show r
intShow (Var l) (Var r) op = show l ++ op ++ show r
intShow (Var l) (    r) op = show l ++ op ++ "(" ++ show r ++ ")"
intShow (    l) (Val r) op = "(" ++ show l ++ ")" ++ op ++ show r
intShow (    l) (Var r) op = "(" ++ show l ++ ")" ++ op ++ show r
intShow (    l) (    r) op = "(" ++ show l ++ ")" ++ op ++ "(" ++ show r ++ ")"

-- Merge two sorted lists into a single sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge xs (y:ys)
    | x == y    = x : merge xs ys
    | otherwise = y : merge (x:xs) ys

-- Finds all unique variable names in an expression and returns them as a list
-- of names
vars :: Expr -> [Name]
vars (Val _) = []
vars (Var n) = [n]
vars (l :+: r) = vars l `merge` vars r
vars (l :-: r) = vars l `merge` vars r
vars (l :*: r) = vars l `merge` vars r
vars (l :/: r) = vars l `merge` vars r
vars (l :%: r) = vars l `merge` vars r

-- Checks whether all names exist in a valuation
isComplete :: [Name] -> Valuation -> Bool
isComplete [] _  = True
isComplete xs [] = False
isComplete xs ys = and (map (/=Nothing) [lookup x ys | x <- xs])

-- Computes the result of an expression given a valuation for its variables
evalExpr :: Expr -> Valuation -> Integer
evalExpr expr val
    | isComplete (vars expr) val = intEvalExpr expr val
    | otherwise                  = error "Incomplete evaluation given"

-- Does the same as evalExpr but assumes that there are no variables undefined
intEvalExpr :: Expr -> Valuation -> Integer
intEvalExpr (Val n) _      = n
intEvalExpr (Var n) v      = sure (lookup n v)
    where sure (Just b) = b
intEvalExpr (e1 :+: e2) v  = intEvalExpr e1 v + intEvalExpr e2 v
intEvalExpr (e1 :-: e2) v  = intEvalExpr e1 v - intEvalExpr e2 v
intEvalExpr (e1 :*: e2) v  = intEvalExpr e1 v * intEvalExpr e2 v
intEvalExpr (e1 :/: e2) v  = intEvalExpr e1 v `div` intEvalExpr e2 v
intEvalExpr (e1 :%: e2) v  = intEvalExpr e1 v `mod` intEvalExpr e2 v

-- Parser interface
toExpr :: String -> Expr
toExpr s = parseE (tokenize s)

-- Turns a string representation of an expression into tokens that are usable
-- by the parser
tokenize :: String -> [String]
tokenize [] = []
tokenize (c:str)
    | isAlpha c = (c : takeWhile isAlpha str) : tokenize (dropWhile isAlpha str)
    | isDigit c = (c : takeWhile isDigit str) : tokenize (dropWhile isDigit str)
    | isOper  c = [c] : tokenize str
    | isSpace c = tokenize (dropWhile isSpace str)
    | isParen c = [c] : tokenize str
    | otherwise = error ("Unrecognised character: " ++ [c])

-- Finds a sub-expression in an expression, does not return the closing bracket
findSubExpr :: [String] -> [String]
findSubExpr = fse 0
    where fse :: Integer -> [String] -> [String]
          fse i [] = []
          fse i (e:es)
            | isCloseParen (head e) && i < 1 = []
            | isCloseParen (head e)          = e : (fse (i-1) es)
            | isOpenParen  (head e)          = e : (fse (i+1) es)
            | otherwise                      = e : (fse i es)

-- General parser for an entire expression
parseE :: [String] -> Expr
parseE [] = Val 0
parseE (e:es) = parseT (fst leftSub) (snd leftSub)
    where leftSub = parseF (e:es)

-- Parse the different operators, (loosely) following the arithmetic priority
-- rules
parseT :: Expr -> [String] -> Expr
parseT expr [] = expr
parseT expr (e:es)
    | e == "*" = parseT (expr :*: fst rightSub) (snd rightSub)
    | e == "/" = parseT (expr :/: fst rightSub) (snd rightSub)
    | e == "%" = parseT (expr :%: fst rightSub) (snd rightSub)
    | e == "+" = expr :+: parseT (fst rightSub) (snd rightSub)
    | e == "-" = expr :-: parseT (fst rightSub) (snd rightSub)
    | otherwise = error ("Invalid operator: " ++ e)
    where rightSub = parseF es

-- Parse fact (but check if the fact is a subexpression)
-- Returns: tuple (parsed expression; remaining tokens to parse)
parseF :: [String] -> (Expr, [String])
parseF (e:es)
    | isDigit (head e) = (Val (read e :: Integer), es)
    | isAlpha (head e) = (Var e, es)
    | e == "(" = (parseE subexpression, drop (1 + length subexpression) es)
    where subexpression = findSubExpr (es)

-- Helper functions for the parser
isOper :: Char -> Bool
isOper c = elem c "+-*/%"

isParen :: Char -> Bool
isParen c = isOpenParen c || isCloseParen c

isOpenParen :: Char -> Bool
isOpenParen c = elem c "("

isCloseParen :: Char -> Bool
isCloseParen c = elem c ")"

-- Simplify expressions
canSimplify :: Expr -> Bool
canSimplify (Val _) = False
canSimplify (Var _) = False
canSimplify e       = null $ vars e

simplifyExpr :: Expr -> Expr
simplifyExpr (Val n) = Val n
simplifyExpr (Var n) = Var n
simplifyExpr e@((e1 :+: e2) :+: e3)
    | canSimplify (e2 :+: e3)= (e1 :+: Val (evalExpr (e2 :+:e3) []))
    | otherwise              = e
simplifyExpr e@((e1 :-: e2) :-: e3)
    | canSimplify (e2 :-: e3)= simplifyExpr (e1 :-: Val (evalExpr (e2 :+: e3) []))
    | otherwise              = e
simplifyExpr e@((e1 :*: e2) :*: e3)
    | canSimplify (e2 :*: e3)= (e1 :*: Val (evalExpr (e2 :*:e3) []))
    | otherwise              = e
simplifyExpr e@((e1 :/: e2) :/: e3)
    | canSimplify (e2 :/: e3)= (e1 :/: Val (evalExpr (e2 :/:e3) []))
    | otherwise              = e
simplifyExpr e@((e1 :%: e2) :%: e3)
    | canSimplify (e2 :%: e3)= (e1 :%: Val (evalExpr (e2 :%:e3) []))
    | otherwise              = e
simplifyExpr e@(e1 :+: e2)
    | canSimplify e          = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :+: e2
    | canSimpe2              = e1 :+: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :-: (Val 0))= simplifyExpr e1
simplifyExpr e@(e1 :-: e2)
    | canSimplify e          = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :-: e2
    | canSimpe2              = e1 :-: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :*: e2)
    | canSimplify e          = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :*: e2
    | canSimpe2              = e1 :*: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :/: e2)
    | canSimplify e          = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :/: e2
    | canSimpe2              = e1 :/: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
simplifyExpr e@(e1 :%: e2)
    | canSimplify e          = Val (evalExpr e [])
    | canSimpe1              = Val (evalExpr e1 []) :%: e2
    | canSimpe2              = e1 :%: Val (evalExpr e2 [])
    | otherwise              = e
    where canSimpe1 = canSimplify e1
          canSimpe2 = canSimplify e2
