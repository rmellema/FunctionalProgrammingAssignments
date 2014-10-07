isSubsequence :: (Eq a) => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence (x:xs) (y:ys)
    | x == y = snd (unzip(zip (x:xs) (y:ys))) == (x:xs) || isSubsequence (x:xs) ys
    | otherwise = isSubsequence (x:xs) ys

divide :: Int -> Int -> [Int]
divide _ 0 = []
divide n m
    | n `mod` m == 0 = [n `div` m]
    | otherwise      = n `div` m : divide ((n `mod` m) *10) m

takeSubsequence :: [a] -> Int -> Int -> [a]
takeSubsequence ns start length = take length $ drop start ns

testBetas :: [Int] -> Int -> Maybe Int
testBetas ns max
    | beta == [] = Nothing
    | otherwise  = Just (head $ beta)
    where isReciprocal xs l = takeSubsequence xs 0 l == takeSubsequence xs l l
          beta = filter (\x -> (isReciprocal ns x)) [1..max]

testAlphas :: [Int] -> Int -> [Int]
testAlphas ns max
    | possibleBetas == [] = []
    | otherwise           = [sure (snd $ head (possibleBetas))] -- Dit is waar ik ben gebleven, possibleBetas is een tupple: (start beta, grote beta)
    where removeAlphas = map (\x -> (x, drop x ns)) [0..max]
          possibleBetas = filter (\x -> maybe False (\y -> True) (snd x)) (map (\x -> (x, testBetas (snd x) max)) removeAlphas)
          sure (Just b) = b

-- filterBeta :: [Int] -> [Int]


foo :: [Int] -> Int -> Int -> (Int, Int)
foo digits a b
    | isSubsequence beta (drop b digits) = foo digits a (max a (b+1))
    | otherwise                          = foo digits (a+1) (max (a+1) b)
    where alpha  = take a digits
          beta   = drop a $ take b digits

makeAlphaBeta :: Int -> ([Int], [Int])
makeAlphaBeta n = ([0], [0])
    where digits = tail $ divide 1 n
