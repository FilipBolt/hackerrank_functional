main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    allStrings <- getMultipleLines n
    putStr $ unlines allStrings
    return ()

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine
        xs <- getMultipleLines (n - 1)
        let ret = (swapLetters x:xs)
        return ret

swapLetters :: String -> String
swapLetters xs = foldr (\((x, y), _) acc -> y : x : acc) "" (filter (even . snd) (zip (zip xs (tail xs)) [0..]))
