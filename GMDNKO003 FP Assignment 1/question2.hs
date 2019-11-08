--The library function last selects the last element of a non-empty list; for example: last [1,2,3,4,5] = 5. Write another definition for the last function in terms of the other library functions. (20%)

--lastn :: Num a => [a] -> a
lastn n = reverse n !! 0
