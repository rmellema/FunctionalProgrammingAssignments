isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
    | even n = False
    | otherwise = and [ n `mod` x /= 0 | x <- [3,5..(n `div` 2)]]

cntPrimes :: Integer -> Int
cntPrimes n = length [x | x <- [1..n], isPrime x]

oddPspTO ::Integer -> Integer -> [Integer]
oddPspTO a upb = [ n | n <- [3, 5..upb], a^(n-1) `mod` n == 1, not (isPrime n)]

expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
    | a >  n    = expmod (a `mod` n) e n
    | e == 1    = a `mod` n
    | e == 0    = 1 `mod` n
    | a == 0    = 0
    | even e	= (expmod (a * a) (e `div` 2) n)
    | otherwise = (expmod a (e - 1) n) * a `mod` n

prop_expmod :: Integer -> Integer -> Integer -> Bool
prop_expmod a e n = ((n <= 0) || (e < 0) || (a < 1)) ||
    (((a ^ e) `mod` n) == (expmod a e n))

oddPspTO' ::Integer -> Integer -> [Integer]
oddPspTO' a upb = [ n | n <- [3, 5..upb], (expmod a (n-1) n) == 1, not (isPrime n)]

