module Compare where

import Expression

data Relop = LessThan | LessEqual | Equal | GreaterEqual | Greater | NotEqual

data Comparison = Cmp Relop Expr Expr

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
