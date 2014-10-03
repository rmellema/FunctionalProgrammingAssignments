type Sudoku = [String]

sudoku :: Sudoku
sudoku = [ "003020600",
           "900305001",
           "001806400",
           "008102900",
           "700000008",
           "006708200",
           "002609500",
           "800203009",
           "005010300" ]

isValidLine :: String -> Bool
isValidLine [] = True
isValidLine (x:xs)
    | x == '0'      = isValidLine xs
    | x `elem` xs   = False
    | otherwise     = isValidLine xs

isValidColumn :: Sudoku -> Int -> Bool
isValidColumn [] _     = True
isValidColumn (x:xs) n = isValidLine [cs !! n | cs <- (x:xs)]

isValidRow :: Sudoku -> Int -> Bool
isValidRow [] _     = True
isValidRow (x:xs) n = isValidLine ((x:xs) !! n)

isValidSudoku :: Sudoku -> Bool
isValidSudoku s = and ((map isValidLine s) ++ (map (isValidColumn s) [1..9]))

isFilledLine :: String -> Bool
isFilledLine = not . elem '0'

isFilledSudoku :: Sudoku -> Bool
isFilledSudoku = and . map isFilledLine

replace :: (Eq a) => a -> a -> [a] -> [a]
replace _ _ []     = []
replace v w (x:xs)
    | v == x    = w : xs
    | otherwise = x : replace v w xs

replaceZero :: Char -> String -> String
replaceZero = replace '0'

repLines :: String -> [String]
repLines [] = []
repLines xs 
    | '0' `elem` xs  = concat $ map repLines
                            $ filter isValidLine [replaceZero x xs | x <- ['1'..'9']]
    | otherwise      = [xs]

allLines :: String -> [String]
allLines [] = []
