module Compare where

import Expression

data Relop = LessThan | LessEqual | Equal | GreaterEqual | Greater | NotEqual

data Comparison = Cmp Relop Expr Expr

cmpToOp :: Comparison -> (a -> Bool)
cmpToOp (Cmp LessThan     _ _ ) = (<)
cmpToOp (Cmp LessEqual    _ _ ) = (<=)
cmpToOp (Cmp Equal        _ _ ) = (==)
cmpToOp (Cmp GreaterEqual _ _ ) = (>=)
cmpToOp (Cmp Greater      _ _ ) = (>)
cmpToOp (Cmp NotEqual     _ _ ) = (/=)

evalCmp :: Comparison -> Valuation -> Bool
evalCmp (Cmp op e1 e2) val = oper (evalExpr e1 val) (evalExpr e2 val)
    where oper = cmpToOp op
