digits :: Integer -> [Integer]
digits x
    | x < 10 = x : []
    | otherwise = (x `mod` 10) : digits (x `div` 10)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]

f :: Integer -> Integer
f = sum .(map factorial).digits

sf :: Integer -> Integer
sf = sum.digits.f

g :: Integer -> Integer
g i = head (filter (\ x -> i == sf x) [1..])

sg :: Integer -> Integer
sg = sum.digits.g

sumsg :: Int -> Integer
sumsg n = sum (take n (map sg [1..]))
