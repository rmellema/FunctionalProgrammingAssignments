import Data.Char
import System.IO

removeWhiteSpace :: String -> String
removeWhiteSpace s = filter (not.isSpace) s

readInput :: IO String
readInput = do input <- hGetContents stdin
               return (removeWhiteSpace input)

main :: IO()
main = do input <- readInput
          putStrLn input
          return ()
