import Data.List
import Data.Map as Map

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
--    allPairs <- getMultipleLines n
--  approach v1 -- too slow
--    putStrLn $ show allPairs
--    putStrLn $ specialPrint $ gcdList allPairs
--  approach v2 -- use maps to get smallest common exponent
    allPairs <- readLines n
    -- factors is [Int]
    let factors = nub $ sort $ concat $ getFactorsFromPairs allPairs
    -- maps is [Map Int Int]
    let maps = castToMaps allPairs
    -- for each factor we look in every map
    let minFactors = allMinFactors factors maps
    -- we filter out factors with 0 exp
    putStrLn $ specialPrint $ pairsForPrintout $ Data.List.filter ((>0) . snd) minFactors

    return ()

-- readLines :: Int -> IO [[(Int, Int)]]
readLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- readLines (n - 1)
        let el = listToPairs $ Data.List.map (read :: String -> Int) (words x)
        let ret = (el : xs)
        return ret

allMinFactors :: [Int] -> [Map Int Int] -> [(Int, Int)]
allMinFactors factors maps = [(f, getMinFactorInMaps maps f) | f <- factors]

getFactorsFromPairs :: [[(Int, Int)]] -> [[Int]]
getFactorsFromPairs [] = []
getFactorsFromPairs (x : xs) = Data.List.map (fst) x : getFactorsFromPairs xs

getMinFactorInMaps :: [Map Int Int] -> Int -> Int
getMinFactorInMaps maps factor = Data.List.foldr (\m acc -> min (maybe 0 (+0) (Map.lookup factor m)) acc ) (maxBound :: Int) maps

listToPairs :: [Int] -> [(Int, Int)]
listToPairs xs = Data.List.foldr (\(idx, n) acc -> (n, xs !! (idx + 1)) : acc) [] (Data.List.filter (even . fst) (zip [0..] xs))

castToMaps :: [[(Int, Int)]] -> [Map Int Int]
castToMaps [] = []
castToMaps (x:xs) = Map.fromList x : castToMaps xs

pairsForPrintout xs = Data.List.foldr (\(x, y) acc -> x : y : acc) [] xs

-- getMultipleLines :: Int -> IO [Integer]
-- getMultipleLines n
--     | n <= 0 = return []
--     | otherwise = do          
--         x <- getLine         
--         xs <- getMultipleLines (n-1)    
--         let ret = (factorToNum (map (read :: String -> Integer) (words x)) :xs)    
--         return ret          
-- 
specialPrint :: [Int] -> String
specialPrint [] = ""
specialPrint (x:xs) = show x ++ " " ++ specialPrint xs
-- 
-- gcdList :: [Integer] -> [Integer]
-- gcdList xs = flatten . freq . prime_factors $ foldr (\x acc -> greatestCommonDivisor x acc) (head xs) xs
-- 
-- greatestCommonDivisor :: Integer -> Integer -> Integer
-- greatestCommonDivisor a b
--   | a > b     = gcd (a - b) b
--   | a < b     = gcd a (b - a)
--   | otherwise = a
-- 
-- factorToNum :: [Integer] -> Integer
-- factorToNum [] = 1
-- factorToNum (f:exp:xs) = f ^ exp * factorToNum xs
-- 
-- 
-- prime_factors :: Integer -> [Integer]
-- prime_factors 1 = []
-- prime_factors n
--   | factors == []  = [n]
--   | otherwise = factors ++ prime_factors (n `div` (head factors))
--   where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]
-- 
-- freq :: [Integer] -> [([Integer], Integer)]
-- freq s = map (\x -> ([head x], toInteger $ length x)) . group . sort $ s
-- 
-- flatten :: Foldable t => t ([Integer], Integer) -> [Integer]
-- flatten s = foldr (\(x, y) acc -> x ++ [y] ++ acc) [] s
