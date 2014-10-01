sudoku :: [String]
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

isValidColumn :: [String] -> Int -> Bool
isValidColumn [] _     = True
isValidColumn (x:xs) n = isValidLine [cs !! n | cs <- (x:xs)]

isValidRow :: [String] -> Int -> Bool
isValidRow [] _     = True
isValidRow (x:xs) n = isValidLine ((x:xs) !! n)

isValidSudoku :: [String] -> Bool
isValidSudoku s = and (concat (map isValidLine s) (map (isValidColumn s) [1..9]))

isFilledLine :: String -> Bool
isFilledLine [] = True
isFilledLine (x:xs)
    | x == '0'  = False
    | otherwise = isFilledLine xs

isFilledSudoku :: [String] -> Bool
isFilledSudoku s = and (map isFilledLine s)

repZeroWith :: String -> Char -> String
repZeroWith [] _    = []
repZeroWith (x:xs) n
    | x == '0'      = n : xs
    | otherwise     = x : repZeroWith xs n

repLines :: String -> [String]
repLines [] = []
repLines xs 
    | '0' `elem` xs  = concat (map repLines (filter isValidLine (map (repZeroWith xs) ['1'..'9'])))
    | otherwise      = [xs]

allLines :: String -> [String]
allLines [] = []
