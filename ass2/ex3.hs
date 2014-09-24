powers :: Integer -> [Integer]
powers n = map (n^) [2..]

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	| x == y = x : merge xs ys
	| x < y = x : merge xs (y:ys)
	| otherwise = y : merge (x:xs) ys

distinctPowers :: Integer -> Int -> [Integer]
distinctPowers m n =
	foldr merge [] ((map (take n) (map powers [2..m])))
