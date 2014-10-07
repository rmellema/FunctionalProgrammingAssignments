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

foo :: [Int] -> Int -> Int -> (Int, Int)
foo digits a b
    | 
    | isSubsequence beta (drop b digits) = foo digits a (max a (b+1))
    | otherwise                          = foo digits (a+1) (max (a+1) b)
    where alpha  = take a digits
          beta   = drop a $ take b digits

makeAlphaBeta :: Int -> ([Int], [Int])
makeAlphaBeta n = 
    where digits = tail $ divide 1 n
