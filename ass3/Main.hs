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

-- Given a constraint satisfaction problem, returns a 2-tuple containing the
-- intervals in the first member and the constraints in the second member
splitCSP :: String -> (String, String)
splitCSP str = (domain, constr)
    where domain = takeWhile (/= '}') (drop ldomain str)
          constr = (tail.init) (drop (length domain + ldomain + lconstr) str)
          ldomain = length "domains{"
          lconstr = length "constraints{"

-- Splits a string delimited by a character into its substrings
split :: Char -> String -> [String]
split _ [] = []
split c (str) = takeWhile (/= c) str : split c rem
    where end = (dropWhile (/= c) str)
          rem = if null end then [] else tail end

-- Parses a single interval and returns a 2-tuple containing the name of the
-- interval and the size of the interval. Because there are no regexes in
-- Haskell this function is a bit FUBAR.
parseDomain :: String -> (Name, Domain)
parseDomain str = (name, domain)
    where name   = takeWhile (isAlpha) str
          first  = read (takeWhile (isDigit) ((drop (length name + lbs)) str))
          second = read ((init.tail.tail) (dropWhile (/= '.') str))
          lbs    = length "<-["
          domain = [first .. second]

main :: IO()
main = do input <- readInput
          putStr "solutions = "
          putStrLn (show (solveCSP input))
          return ()
