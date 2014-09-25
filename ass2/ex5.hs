isValidLine :: [Int] -> Bool
isValidLine [] = True
isValidLine (x:xs)
    | x == 0        = isValidLine xs
    | x `elem` xs   = False
    | otherwise     = isValidLine xs

isValidIntSudoku :: [[Int]] -> Bool
isValidIntSudoku s = and (map isValidLine s)

isFilledLine :: [Int] -> Bool
isFilledLine [] = True
isFilledLine (x:xs)
    | x == 0    = False
    | otherwise = isFilledLine xs

isFilledIntSudoku :: [[Int]] -> Bool
isFilledIntSudoku s = and (map isFilledLine s)

toIntLine :: String -> [Int]
toIntLine [] = []
toIntLine (x:xs) = (fromEnum x - offset) : toIntLine xs
    where offset = fromEnum '0'

toIntSudoku :: [String] -> [[Int]]
toIntSudoku = map toIntLine

