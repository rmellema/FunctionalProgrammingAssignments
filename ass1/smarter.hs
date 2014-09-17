-- The order from the exercise description
order' :: Integer -> Integer -> Integer
order' a p = ord a (a `mod` p) 1 p
    where ord a e k p = if e == 1 then k else ord a (a*e `mod` p) (k+1) p

primeFactors :: Integer -> [Integer]
primeFactors 0 = []
primeFactors 1 = []
primeFactors n = pF n 2
    where pF a p
            | a == p         = [p]
            | a `mod` p == 0 = p : pF (a `div` p) p 
            | a < p          = error "Something went Horribly wrong..."
            | p == 2         = pF a 3
            | otherwise      = pF a (p+2)

order :: Integer -> Integer -> Integer
order a p =
   where factors = primeFactors (p-1)
         dfsord a p facs 
