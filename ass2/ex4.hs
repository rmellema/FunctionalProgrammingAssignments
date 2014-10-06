-- Generate palindromic numbers
makePalindrome :: Integer -> [Integer]
makePalindrome n = takeWhile (<n) (map toNumber (seedPalindrome ++ makePalInfinite seedPalindrome))

seedPalindrome :: [[Integer]]
seedPalindrome = [[x] | x <- [1..9]] ++ makeNumberPal []

makePalInfinite :: [[Integer]] -> [[Integer]]
makePalInfinite xs = makeListPal xs ++ makePalInfinite (makeListPal xs)

makeListPal :: [[Integer]] -> [[Integer]]
makeListPal xs = concat (map makeNumberPal xs)

makeNumberPal :: [Integer] -> [[Integer]]
makeNumberPal n = map (\x -> [x] ++ n ++ [x]) [1..9]

toNumber :: [Integer] -> Integer
toNumber xs = tN 0 xs where
    tN n [] = n
    tN n (x:xs) = tN (n*10+x) xs

-- Test for composisity
primes :: [Integer]
primes = 2: sieve [3,5..]

sieve :: [Integer] -> [Integer]
sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n 
    | factors == [] = [n]
    | otherwise = factors ++ primeFactors (n `div` (head factors))
    where factors = take 1 (filter (\x -> (n `mod` x) == 0) (takeWhile (<=(div n 2)) primes))

isPalindromicComposite :: Integer -> Bool
isPalindromicComposite n = length (primeFactors n) == 2

numberOfPalindromicComposites :: Integer -> Int
numberOfPalindromicComposites n = length (filter isPalindromicComposite (makePalindrome n))

primeFactorsList :: [Integer] -> [Integer]
primeFactorsList xs = map fst (makeTuple xs) where
    makeTuple ys = map (\x -> if (x `mod` 2 == 0) then (0, x) else (1, div x 2)) ys
