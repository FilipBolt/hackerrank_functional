main :: IO()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    targets <- readLines n
    putStr $ specialPrint targets
    return ()


readLines :: Int -> IO[Integer] 
readLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        let el = map read (words x) :: [Integer]
        xs <- readLines (n - 1)
        let n = el !! 0
        let k = el !! 1
        let ret = choose n k `mod` (100000000 + 7) : xs
        return ret

choose :: Integer -> Integer -> Integer
choose n k
  | k > n           = undefined
  | k == 0          = 1
  | k > (n `div` 2) = choose n (n-k)
  | otherwise       = n * ((n-1) `choose` (k-1)) `div` k


specialPrint :: [Integer] -> String
specialPrint [] = []
specialPrint (x:xs) = show x ++ "\n" ++ specialPrint xs
