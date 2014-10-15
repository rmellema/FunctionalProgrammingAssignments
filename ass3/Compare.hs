module Compare(Comparison, evalCmp, toComparison, Valuation) where

import Expression

data Relop = LessThan | LessEqual | Equal | GreaterEqual | Greater | NotEqual
    deriving Show

data Comparison = Cmp Relop Expr Expr
    deriving Show

cmpToOp :: (Ord a) => Relop -> (a -> a -> Bool)
cmpToOp LessThan     = (<)
cmpToOp LessEqual    = (<=)
cmpToOp Equal        = (==)
cmpToOp GreaterEqual = (>=)
cmpToOp Greater      = (>)
cmpToOp NotEqual     = (/=)

evalCmp :: Comparison -> Valuation -> Bool
evalCmp (Cmp op e1 e2) val = oper (evalExpr e1 val) (evalExpr e2 val)
    where oper = cmpToOp op

toComparison :: String -> Comparison
toComparison str = Cmp oper (toExpr left) (toExpr right)
    where left  = leftHand str
          op    = findOp (drop (length left) str)
          right = drop ((length left) + (length op)) str
          oper
            | op == "<"  = LessThan
            | op == "<=" = LessEqual
            | op == "="  = Equal
            | op == ">=" = GreaterEqual
            | op == ">"  = Greater
            | op == "#"  = NotEqual
            | otherwise = error "Incorrect comparison operator"

leftHand :: String -> String
leftHand [] = []
leftHand (s:str)
    | isComp s  = []
    | otherwise = s : (leftHand str)

findOp :: String -> String
findOp [] = []
findOp (s:str)
    | isComp s  = s : (findOp str)
    | otherwise = []

isComp :: Char -> Bool
isComp c = c == '<' || c == '=' || c == '>' || c == '#'
