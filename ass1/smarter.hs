import Data.List

expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
    | a >  n    = expmod (a `mod` n) e n
    | e == 1    = a `mod` n
    | e == 0    = 1 `mod` n
    | a == 0    = 0
    | even e	= (expmod (a * a) (e `div` 2) n)
    | otherwise = (expmod a (e - 1) n) * a `mod` n

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
order a p = head [ x | x <- nub (map product (subsequences factors)), expmod a x p == 1]
   where factors = primeFactors (p-1)
