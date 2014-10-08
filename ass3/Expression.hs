module Expression(Expr, vars, evalExpr) where
import Types
import Valuation

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
