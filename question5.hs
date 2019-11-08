--5. Using split, define a function: exprs :: [ Int ] → [ Expr ] that returns all expressions whose list of values is a given list. For example, exprs [1, 2, 3] should return all e for which values e = [1, 2, 3]. (24%)

data Expr = Val Int | App Op Expr Expr
data Op = Add | Sub | Mul | Div
--eg. of output: [1+2+3],[1+2*3],[1*2+3],[1*2*3] -> 6,7,5,6 respectively

--See slide 114
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = do
   [e | (ls, rs) <- split ns
              , l       <- exprs ls
              , r       <- exprs rs
              , e       <- combine l r]

instance Show Expr where
    show (App Add x y) = show x ++ "+" ++ show y
    show (App Mul x y) = show x ++ "*" ++ show y
    show (App Div x y) = show x ++ "/" ++ show y
    show (App Sub x y) = show x ++ "-" ++ show y
    show (Val x)   = show x

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

split :: [Int] -> [([Int], [Int])]
split xs = do
   let l = length xs
   split2 (l-1) xs
   
split2 1 xs = do
   let l = length xs
   [ (take (l-1) xs , drop (l-1) xs) ]
split2 x xs = do
   let l = length xs
   [ (take (l-x) xs , drop (l-x) xs) ] ++ split2 (x-1) xs
