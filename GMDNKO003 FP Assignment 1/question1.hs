--Define a function product that produces the product of a list of numbers. For example: product [2,3,4], should produce the solution: 24. (20%)

productn :: Num a => [a] -> a
productn [] =  1
productn (n:ns) = n*productn ns 
