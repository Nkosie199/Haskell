--1. Suppose that arithmetic expressions built up from integers, addition and multiplication are represented using the following types:
--data Expr = Val Int | App Op Expr Expr
--data Op = Add | Mul
--Define functions: eval :: Expr → Int values :: Expr → [ Int ] that respectively evaluate an expression to its integer value, and return the list of integer values contained in an expression. (12%)

data Expr = Val Int | App Op Expr Expr
data Op = Add | Mul
--eg. of terminal commands:...
--eval (Val 3) -> 3
--eval (App Add (Val 3) (Val 2)) -> 5
--eval (App Mul (Val 3) (Val 3))
--eval (App Add (Val 3) (Val 3))
eval :: Expr -> Int
eval (Val n) = n
eval (App Add x y) = sum (values x ++ values y)
eval (App Mul x y) = product (values x ++ values y)

values :: Expr -> [Int]
values (Val n) = [n]

-- see Haskell textbook example (pg. 218)
