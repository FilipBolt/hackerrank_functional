main :: IO()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    targets <- readLines n
    putStr $ specialPrint targets
    return ()


readLines :: Int -> IO [Integer]
readLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        let el = read x :: Integer
        xs <- readLines (n - 1)
        let ret = catalanNumber el `mod` (100000000 + 7) : xs
        return ret

catalanNumber :: Integer -> Integer
catalanNumber n = toInteger $ (choose (2 * n) n) `div` (n + 1)

choose :: Integer -> Integer -> Integer
choose n k
  | k > n           = undefined
  | k == 0          = 1
  | k > (n `div` 2) = choose n (n-k)
  | otherwise       = n * ((n-1) `choose` (k-1)) `div` k


specialPrint :: [Integer] -> String
specialPrint [] = []
specialPrint (x:xs) = show x ++ "\n" ++ specialPrint xs
