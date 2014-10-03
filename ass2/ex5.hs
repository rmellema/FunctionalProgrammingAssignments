lastDigits :: Int -> Integer -> [Integer]
lastDigits _ 0 = []
lastDigits n d = reverse (lD (mod number max) d)
    where number = sum (take n [expmod x x max | x <- [1..]])
          lD x 0 = []
          lD x d = (x `mod` 10) : lD (x `div` 10) (d-1)
          max = (10^d)

expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n
    | e == 0    = 1
    | even e    = (expmod ((a * a) `mod` n) (e `div` 2) n)
    | otherwise = (expmod a (e - 1) n) * a `mod` n
