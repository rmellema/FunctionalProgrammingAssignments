divide :: Int -> Int -> [Int]
divide _ 0 = []
divide n m
    | n `mod` m == 0 = [n `div` m]
    | otherwise      = n `div` m : divide ((n `mod` m) *10) m

takeSubsequence :: [a] -> Int -> Int -> [a]
takeSubsequence ns start length = take length $ drop start ns

testBetas :: [Int] -> Int -> Maybe Int
testBetas ns limit
    | beta == [] = Nothing
    | otherwise  = Just (head beta)
    where isReciprocal xs l = takeSubsequence xs 0 l == takeSubsequence xs l l
          beta = filter (\x -> (isReciprocal ns x)) [1..limit]

testAlphas :: [Int] -> Int -> Int -> Int
testAlphas ns limit betaLimit
    | possibleBetas == [] = 0
    | otherwise           = sure (head (possibleBetas))
    where removeAlphas  = map (\x -> drop x filterList) [0..limit]
          possibleBetas = filter (\x -> maybe False (\y -> True) x) (map (\x -> testBetas x betaLimit) (filter (not.null) removeAlphas))
          sure (Just b) = b
          filterList    = dropWhile (<=0) ns

repetitiveReciprocal :: Int -> Int -> Int
repetitiveReciprocal m n = fst (foldr (largestReciprocal) (1, 1) reciprocals)
	where
		largestReciprocal x y = (if snd x < snd y then y else x)
		reciprocals = map (\x -> (x, testAlphas (divide 1 x) 10 1000)) [m..n]
