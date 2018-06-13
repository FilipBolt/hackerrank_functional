import Data.List 

main :: IO ()
main = do
    x_temp <- getLine
    let sum = read x_temp :: Int
    y_temp <- getLine
    let power = read y_temp :: Int
    let res = f sum power
    putStrLn $ show res
    return ()

f sum power = count (candidates sum power) sum

candidates :: Int -> Int -> [Int]
candidates n k = takeWhile (<=n) [x^k | x <- [1..]]

count :: [Int] -> Int -> Int
count xs n = foldr (\x acc -> if sum x == n then acc + 1 else acc) 0 (subsequences xs) 
