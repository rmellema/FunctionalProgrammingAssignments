import Data.List

expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
    | a >  n    = expmod (a `mod` n) e n
    | e == 1    = a `mod` n
    | e == 0    = 1 `mod` n
    | a == 0    = 0
    | even e    = (expmod (a * a) (e `div` 2) n)
    | otherwise = (expmod a (e - 1) n) * a `mod` n

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

mergeAsc :: [Integer] -> [Integer] -> [Integer]
mergeAsc [] ys = ys
mergeAsc xs [] = xs
mergeAsc (x:xs) (y:ys)
    | x >= y    = y : mergeAsc (x:xs) ys
    | otherwise = x : mergeAsc xs (y:ys)

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
order a p
    | null orders   = 1
    | otherwise     = head orders
   where factors = primeFactors (p-1)
         candidates = insertionSort (nub (map product (subsequences factors)))
         orders = [ x | x <- candidates, expmod a x p == 1]

--oddPspTO :: Integer -> Integer -> [Integer]
--oddPspTO a upb
oddPspTO' a upb = [ n | n <- [3, 5..upb], let p = head (primeFactors n), let q = (n `div` p), (a < p), q `mod` (order a p) == 1, (expmod a (n-1) n) == 1, not (isPrime' n)]

oddPspTO :: Integer -> Integer -> [Integer]
oddPspTO a upb = [ n | n <- nub (ns primes),(expmod a (n-1) n) == 1, not (isPrime' n)]
    where primes  = 2 : [ x | x <- [3,5..(ceiling (sqrt (fromIntegral upb)))], isPrime' x]
          ns []   = []
          ns (p:ps) = mergeAsc ([p * k * e + p | k <- [1..(div (div upb p) e)]]) (ns ps)
            where e = order a p

isPrime' :: Integer -> Bool
isPrime' 1 = False
isPrime' 2 = True
isPrime' n
    | even n = False
    | otherwise = and [ n `mod` x /= 0 | x <- [3,5..(n `div` 2)]]
