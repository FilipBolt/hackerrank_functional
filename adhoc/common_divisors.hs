import Data.List

main :: IO()
main = do
    t_temp <- getLine
    let testCases = read t_temp :: Int
    result <- readLines testCases
    putStr $ prettyPrint $ reverse result
    return ()


readLines :: Int -> IO [Int]
readLines n
    | n <= 0 = return []
    | otherwise = do
        xs <- readLines (n - 1)
        xsRaw <-  getLine
        let x = map (read :: String -> Integer) (words xsRaw)
        let sol = length $ (divisors $ x !! 0) `intersect` (divisors $ x !! 1)
        return (sol : xs)

factorsSqrt :: Integer -> [Integer]
factorsSqrt n = [1..maxDiv]
  where maxDiv = toInteger $ floor sq
        sq = sqrt (fromInteger n)


divisors :: Integer -> [Integer]
divisors n = foldr (addFactors n) [] factors
  where factors = factorsSqrt n

addFactors :: Integer -> Integer -> [Integer] -> [Integer]
addFactors n x acc
  | n `mod` x == 0 && n `div` x == x = [x] ++  acc
  | n `mod` x == 0                   = [x, toInteger (n `div` x)] ++ acc
  | otherwise                        = acc


prettyPrint :: [Int] -> String
prettyPrint xs = unlines $ map (\x -> show x) xs
