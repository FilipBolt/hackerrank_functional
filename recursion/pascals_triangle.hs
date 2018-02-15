main :: IO()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    let all = map (triangle) [0..(n-1)]
    let out = map (floatListToInt) all 
    putStr $ unlines out
    return ()


facts :: [Double]
facts = 1 : 2 : zipWith (*) [3..] (tail facts)

nOverR :: Int -> Int -> Double
nOverR _ 0 = 1
nOverR n r = if n == r then 1 else allFacts n / (allFacts r * allFacts (n - r))
    where allFacts x = (take n facts) !! (x - 1)

triangle :: Int -> [Double]
triangle n = map (nOverR n) [0..n]

floatListToInt :: [Double] -> String
floatListToInt = unwords . map (\x -> show $ floor x)
