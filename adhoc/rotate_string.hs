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
        let ret = (unwords (tail (getAllRotations x) ++ [head (getAllRotations x)] ) : xs)
        return ret
    where getAllRotations x = take (length x) (iterate rotateString x)

rotateString :: String -> String
rotateString (x:xs) = xs ++ [x]
