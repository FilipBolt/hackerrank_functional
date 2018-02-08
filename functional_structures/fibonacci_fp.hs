main :: IO()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    targets <- readLines n
    let t = divide $ map (((!!) fibos)) targets
    putStr $ specialPrint t
    return ()

readLines :: Int -> IO [Int] 
readLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        let el = read x :: Int
        xs <- readLines (n - 1)
        let ret = el : xs
        return ret


fibos :: [Integer]
fibos = serie 0 1
    where serie i j = i : serie j (i+j)


divide :: [Integer] -> [Integer]
divide = map (`mod` (100000000 + 7))    

specialPrint [] = []
specialPrint (x:xs) = show x ++ "\n" ++ specialPrint xs
