isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n
    | even n = False
    | otherwise = and [ n `mod` x /= 0 | x <- [3,5..(n `div` 2)]]
