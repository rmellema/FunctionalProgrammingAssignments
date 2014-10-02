lastDigits :: Int -> Integer -> [Integer]
lastDigits _ 0 = []
lastDigits n d = lD number d
    where number = sum (take n [expmod x x (10^d) | x <- [1..]])
          lD x 0 = []
          lD x d = (x `mod` 10) : lD (x `div` 10) (d-1)

expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
    | a >  n    = expmod (a `mod` n) e n
    | e == 1    = a `mod` n
    | e == 0    = 1 `mod` n
    | a == 0    = 0
    | even e    = (expmod (a * a) (e `div` 2) n)
    | otherwise = (expmod a (e - 1) n) * a `mod` n
