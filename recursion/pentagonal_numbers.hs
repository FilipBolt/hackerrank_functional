main :: IO()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    nums <- readLines n
    putStr $ unlines $ map (\x -> show x) nums
    return ()

readLines :: Int -> IO [Integer]
readLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        let num = read x :: Double
        xs <- readLines (n - 1)
        return (round (calculatePentagonal num) : xs)

-- p_n = \frac{3n^2-n}{2}
-- https://en.wikipedia.org/wiki/Pentagonal_number
calculatePentagonal :: Double -> Double
calculatePentagonal 1 = 1
calculatePentagonal n = (3 * (n ** 2) - n) / 2
