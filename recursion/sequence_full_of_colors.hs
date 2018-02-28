import Data.List

main :: IO()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    result <- readLines t
    putStr $ specialPrint result 
    return ()


readLines :: Int -> IO [Bool]
readLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- readLines (n - 1)
        return (((fst (differenceCheck x)) && equalCnt x) : xs)


differenceCheck :: String -> (Bool, (Int, Int, Int, Int))
differenceCheck str = foldr (processColor) (True, (0, 0, 0, 0)) str

processColor :: Char -> (Bool, (Int, Int, Int, Int)) -> (Bool, (Int, Int, Int, Int))
processColor c (res, (r, g, y, b))
  | c == 'R' && (checkCondition (r + 1) g y b)     = (True && res, (r + 1, g, y, b))
  | c == 'R' && not (checkCondition (r + 1) g y b) = (False && res, (r + 1, g, y, b))
  | c == 'G' && (checkCondition r (g + 1) y b)     = (True && res, (r, g + 1, y, b))
  | c == 'G' && not (checkCondition r (g + 1) y b) = (False && res, (r, g + 1, y, b))
  | c == 'Y' && (checkCondition r g (y + 1) b)     = (True && res, (r, g, y + 1, b))
  | c == 'Y' && not (checkCondition r g (y + 1) b) = (False && res, (r, g, y + 1, b))
  | c == 'B' && (checkCondition r g y (b + 1))     = (True && res, (r, g, y, b + 1))
  | c == 'B' && not (checkCondition r g y (b + 1)) = (False && res, (r, g, y, b + 1))


-- (0, 0, 0, 0)
equalCnt :: String -> Bool
equalCnt str = (r == g) && (y == b)
  where (r, g, y, b) = foldr (cnt) (0, 0, 0, 0) (group $ sort str)

cnt :: String -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
cnt x (r, g, y, b)
  | start == 'R' = (r + length x, g, y, b)
  | start == 'G' = (r, g + length x, y, b)
  | start == 'Y' = (r, g, y + length x, b)
  | start == 'B' = (r, g, y, b + length x)
  where start = head x


checkCondition :: Int -> Int -> Int -> Int -> Bool
checkCondition r g y b = (abs (y - b)) <= 1 && (abs (r - g)) <= 1

-- going to have r, g, y, b
-- third element of a tuple
trd (_, _, x, _) = x

-- fourth element of a tuple
frt (_, _, _, x) = x


specialPrint :: [Bool] -> String
specialPrint [] = ""
specialPrint (x:xs) = show x ++ "\n" ++ specialPrint xs
