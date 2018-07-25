main :: IO()
main = do
    n_temp <- getLine
    arrTemp <- getLine
    let arr = map (read :: String -> Integer) . words $ arrTemp
    let result = lcmm arr
    putStrLn $ show result
    return ()


lcmm [] = 1
lcmm (x : xs) = lcm x (lcmm xs)
