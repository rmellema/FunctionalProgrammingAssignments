import Data.Char
import System.IO
import Compare
import Types
import Valuation

removeWhiteSpace :: String -> String
removeWhiteSpace s = filter (not.isSpace) s

readInput :: IO String
readInput = do input <- hGetContents stdin
               return (removeWhiteSpace input)

solveCSP :: String -> [Valuation]
solveCSP str = [d | d <- domains, and $ map (\x -> evalCmp x d) constraints]
    where (domain, constr) = splitCSP str
          domains          = valuations (map parseDomain (split ',' domain))
          constraints      = map toComparison (split ',' constr)

splitCSP :: String -> (String, String)
splitCSP str = (domain, constr)
    where domain = takeWhile (/= '}') (drop ldomain str)
          constr = (tail.init) (drop (length domain + ldomain + lconstr) str)
          ldomain = length "domains{"
          lconstr = length "constraints{"

split :: Char -> String -> [String]
split _ [] = []
split c (str) = takeWhile (/= c) str : split c rem
    where end = (dropWhile (/= c) str)
          rem = if null end then [] else tail end

parseDomain :: String -> (Name, Domain)
parseDomain str = (name, domain)
    where name   = takeWhile (isAlpha) str
          first  = read (takeWhile (isDigit) ((drop (length name + lbs)) str)) :: Integer
          second = read ((init.tail.tail) (dropWhile (/= '.') str)) :: Integer
          lbs    = length "<-["
          domain = [first .. second]

main :: IO()
main = do input <- readInput
          putStrLn (show (solveCSP input))
          return ()
