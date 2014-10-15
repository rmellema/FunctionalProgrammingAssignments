module Compare where

import Expression

data Relop = LessThan | LessEqual | Equal | GreaterEqual | Greater | NotEqual

data Comparison = Cmp Relop Expr Expr

