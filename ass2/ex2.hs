mults :: Integer -> [Integer]
mults n = map (*n) [1..]

multiples :: [Integer] -> [Integer]
multiples xs = foldr merge [] (map mults xs)

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <  y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

multsum :: Integer -> [Integer] -> Integer
multsum n xs = sum (takeWhile (<n) (multiples xs))
