isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
    | even n = False
    | otherwise = and [ n `mod` x /= 0 | x <- [3,5..(n `div` 2)]]

cntPrimes :: Integer -> Int
cntPrimes n = length [x | x <- [1..n], isPrime x]

