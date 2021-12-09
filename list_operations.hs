-- naturals: The infinite list of natural numbers.
naturals :: [Integer]
naturals = [0,1..]

-- interleave: Interleave two lists, cutting off on the shorter list.
interleave :: [a] -> [a] -> [a]
interleave _ [] = [] 
interleave [] _ = []
interleave (x:xs) (y:ys) = x:(y:(interleave xs ys))


-- integers: The infinite list of integers. Ordered as [0, -1, 1, -2, 2, -3, 3, -4, 4...].
integers :: [Integer]
integers = let negative_n = [-x | x<-naturals, x /= 0]
            in interleave naturals negative_n
