smallestMultiple' :: Integer -> Integer
smallestMultiple' n = sM (n-1) (n-2)
	where sM n m
		| m <= 2 = n
		| n `mod` m /= 0 = sM (n*m) (m-1)
		| otherwise = sM n (m-1)

smallestMultiple :: Integer -> Integer
smallestMultiple n = foldr sM (n-1) [2..(n-1)]
	where sM n m
		| m `mod` n /= 0 = n*m
		| otherwise = m
