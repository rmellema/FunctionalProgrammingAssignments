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
