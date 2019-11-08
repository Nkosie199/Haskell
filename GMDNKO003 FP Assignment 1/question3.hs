--Using library functions, define a function halve :: [a] − > ([a],[a]) that splits an even length list into two halves. For example: halve [1,2,3,4,5,6], should produce: ([1,2,3],[4,5,6]). (20%)

--halve :: Num a => [a] −> ([a],[a])
halve n = do
   let l = length n 
   let h = l `div` 2
   --now it should read from index 0..h & h+1..l
   let x = take h n
   let y = drop h n
   (x,y)
