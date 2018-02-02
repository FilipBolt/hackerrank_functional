main :: IO ()
main = do
    str1 <- getLine
    str2 <- getLine
    let output = foldr (\(x, y) acc -> x : y : acc) "" (zip str1 str2)
    putStrLn output
    return ()
