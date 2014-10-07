-- Generate palindromic numbers
makePalindrome :: Integer -> [Integer]
makePalindrome n = takeWhile (<n) (map toNumber (filter ((/=0).head) (seedPalindrome ++ makePalInfinite seedPalindrome)))
    where seedPalindrome = [[x] | x <- [0..9]] ++ makeNumberPal []
          makePalInfinite xs = makeListPal xs ++ makePalInfinite (makeListPal xs)
          makeListPal xs = concat (map makeNumberPal xs)
          makeNumberPal n = map (\x -> [x] ++ n ++ [x]) [0..9]
          toNumber xs = tN 0 xs 
            where tN n [] = n
                  tN n (x:xs) = tN (n*10+x) xs

-- Test for composisity (it is a word now)
primes :: [Integer]
primes = 2: sieve [3,5..]
    where sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n 
    | factors == [] = [n]
    | otherwise = factors ++ primeFactors (n `div` (head factors))
    where factors = take 1 (filter (\x -> (n `mod` x) == 0) (takeWhile (<=((ceiling.sqrt.fromIntegral) n)) primes))

isPalindromicComposite :: Integer -> Bool
isPalindromicComposite n
    | length factors < 2 = False
    | otherwise = n `div` head factors `div` (head.tail) factors == 1
    where factors = take 2 (primeFactors n)

numberOfPalindromicComposites :: Integer -> Int
numberOfPalindromicComposites n = length (filter isPalindromicComposite (makePalindrome n))
