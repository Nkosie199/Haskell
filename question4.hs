--4. Define a function: split :: [ Int ] → [([Int], [Int])] that returns all splits of a list into two non-empty parts that append to give the original list. For example, split [1, 2, 3, 4] should return:
--[ ( [1], [2, 3, 4] ), ( [1, 2], [3, 4]), ( [1, 2, 3], [4] ) ] (16%)

split :: [Int] -> [([Int], [Int])]
split xs = do
   --split at indices [1..n-1]
   let l = length xs
   split2 (l-1) xs
   
--this function takes an int cutoff as well
split2 1 xs = do
   let l = length xs
   [ (take (l-1) xs , drop (l-1) xs) ]
split2 x xs = do
   let l = length xs
   [ (take (l-x) xs , drop (l-x) xs) ] ++ split2 (x-1) xs
