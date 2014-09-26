primes :: [Integer]
primes = 2: sieve [3,5..]

sieve :: [Integer] -> [Integer]
sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]
