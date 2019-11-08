--6. Using your answers to the previous parts, define a function: solve :: [ Int ] → Int → [ Expr ] that returns all expressions whose list of values is a permutation of the given list and whose value is the given value. For example, solve [1, 2, 3, 4] 10 should return all expressions e for which values e is a permutation of [1, 2, 3, 4] and eval e = 10. (16%)

data Expr = Val Int | App Op Expr Expr
data Op = Add | Sub | Mul | Div

solve :: [ Int ] -> Int -> [ Expr ]
solve xs x = do
   [e | ns <- perms xs
      , e <- exprs ns
      , eval e == x]

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

perms :: [ Int ] -> [ [ Int ] ]
perms [] = [[]]
perms xs = do
   x <- xs
   let y = delete x xs
   ys <- perms y
   return $ x : ys

delete :: Int -> [ Int ] -> [ Int ]
delete n (x:xs) = if n == x then xs else [x] ++ delete n xs

eval :: Expr -> Int
eval (Val n) = n
eval (App Add x y) = sum (values x ++ values y)
eval (App Mul x y) = product (values x ++ values y)
eval x = 1

values :: Expr -> [Int]
values (Val n) = [n]
values (App Add x y) = values x ++ values y
values (App Mul x y) = values x ++ values y
values x = []
