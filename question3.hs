--3. Using delete, define a function: perms :: [ Int ] → [ [ Int ] ]
--that returns all permutations of a list, given by all possible re-orderings of its elements. For example, perms [1, 2, 3] should return: [ [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], [3, 2, 1] ] (20%)

perms :: [ Int ] -> [ [ Int ] ]
perms [] = [[]]
perms xs = do
   x <- xs --generate x iteratively from the list ie. [1,2,3] 
   let y = delete x xs --y is the list excluding x ie. [2,3]
   ys <- perms y --generate ys iteratively from the list excluding x ie. [2,3]
   return $ x : ys --return (x:ys) ie. [1]:[2,3]

delete :: Int -> [ Int ] -> [ Int ]
delete n (x:xs) = if n == x then xs else [x] ++ delete n xs
