--Consider a function safetail :: [a] −> [a] that behaves in the same way as the tail function except that it maps the empty list to itself rather than producing an error. Using tail and the function null :: [a] −> Bool that decides if a list is empty or not, define safetail using only: (1) a conditional expression, (2) guarded equations, and (3) pattern matching. Defining three different functions for solving these three different problems is an acceptable approach. (40%)

--safetail :: [a] −> [a]
safetail n = do
   if nulln n then safetails [] else safetails n

safetails [] = []
safetails (x:xs) = xs

--nulln :: [a] -> Bool
nulln n 
   | n == [] = True
   | otherwise = False
