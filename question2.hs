--2. Define a function: delete :: Int → [ Int ] → [ Int ]
--that deletes the first occurrence (if any) of a value from a list. For example, delete 2 [1, 2, 3, 2] should give the result [1, 3, 2]. (12%)

delete :: Int -> [ Int ] -> [ Int ]
delete n (x:xs) = if n == x then xs else [x] ++ delete n xs
