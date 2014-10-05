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

putSudoku :: Sudoku -> IO ()
putSudoku = putStr . unlines

isFilledLine :: String -> Bool
isFilledLine = not . elem '0'

isFilledSudoku :: Sudoku -> Bool
isFilledSudoku = and . map isFilledLine

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i v = take i xs ++ (v: drop (i+1) xs)

replaceAt2D :: [[a]] -> Int -> Int -> a -> [[a]]
replaceAt2D xss r c v = take r xss ++ (replaceAt (xss !! r) c v : drop (r+1) xss)

validNumberInRow :: String -> Char -> Bool
validNumberInRow r n = not $ elem n r

validNumberInSudoku :: Sudoku -> Char -> Int -> Int -> Bool
validNumberInSudoku sud num row col = 
    and [validNumberInRow (sud !! row) num, 
        validNumberInRow [s !! col | s <- sud] num,
        validNumberInRow (concat [take 3 (drop bCol r) | r <- [sud !! x | x <- [bRow..eRow]]]) num]
    where bRow = row - (row `mod` 3)
          eRow = bRow + 2
          bCol = col - (col `mod` 3)


solutionsSudoku :: Sudoku -> [Sudoku]
solutionsSudoku []      = []
solutionsSudoku sudoku  = solve sudoku 0 0
    where solve [] _ _  = []
          solve sud 9 _ = [sud]
          solve sud r 9 = solve sud (r+1) 0
          solve sud r c
            | (sud !! r) !! c /= '0' = solve sud r (c+1)
            | otherwise              = concat $ map (\x -> solve x r (c+1)) sols
            where sols = map (replaceAt2D sud r c)
                             (filter (\x -> validNumberInSudoku sud x r c) ['1'..'9'])
    

isCorrectSudoku :: Sudoku -> Bool
isCorrectSudoku sud = isCorrect (solutionsSudoku sud)
    where isCorrect []      = False
          isCorrect [x]     = True
          isCorrect (x:xs)  = False

minimalSudoku :: Sudoku -> Sudoku
minimalSudoku sud = minSud sud 0 0
    where minSud [] _ _  = []
          minSud sud 9 _ = sud
          minSud sud r 9 = minSud sud (r+1) 0
          minSud sud r c
            | (sud !! r) !! c == '0'                                = nextSud'
            | isCorrectSudoku nextSud && (nextCount > nextCount')   = nextSud
            | isCorrectSudoku nextSud'                              = nextSud'
            | otherwise                                             = sud
            where nextSud    = minSud (replaceAt2D sud r c '0') r (c+1)
                  nextSud'   = minSud sud r (c+1)
                  nextCount  = sum $ map (countZeros 0) nextSud
                  nextCount' = sum $ map (countZeros 0) nextSud'
          countZeros n []    = n
          countZeros n (x:xs) 
            | x == '0'   = countZeros (n+1) xs 
            | otherwise  = countZeros n xs 
