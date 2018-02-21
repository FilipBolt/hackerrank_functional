import Data.List
import Data.Function


main :: IO()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    result <- readLines t
    let uniqued = map uniqueList result
    let forPrint = map specialPrint uniqued
    putStr $ unlines forPrint
    return ()


readLines :: Int -> IO [[Int]]
readLines n
    | n <= 0 = return []
    | otherwise = do
        arrTemp <- getLine
        arrTemp2 <- getLine
        xs <- readLines (n - 1)

        let arr = map (read :: String -> Int) . words $ arrTemp
        let k = arr !! 1

        let nums = map (read :: String -> Int) . words $ arrTemp2

        return (filterGrouped k (getOrigSorted nums) : xs)

uniqueList :: [Int] -> [Int]
uniqueList l
  | length l > 0 = l
  | otherwise    = [-1]

specialPrint :: [Int] -> String
specialPrint [] = ""
specialPrint (x:xs) = show x ++ " " ++ specialPrint xs

getOrigSorted :: [Int] -> [[(Int, Int)]]
getOrigSorted l = groupBy (\x y -> fst x == fst y) $ sortBy (compare `on` fst) (zip l [0..])

filterGrouped :: Int -> [[(Int, Int)]] -> [Int]
filterGrouped k xs = map (fst) $ sortBy (compare `on` snd) $ map (head) $ filter (\x -> length x >= k) xs

