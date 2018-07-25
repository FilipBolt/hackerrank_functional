import Data.Char


main :: IO()
main = do
    arrTemp <- getLine
    let arr = map (read :: String -> Integer) . words $ arrTemp
    let n = arr !! 0
    let k = arr !! 1
    let result = superDigit n True k
    putStrLn $ show result
    return ()

superDigit :: Integer -> Bool -> Integer -> Integer 
superDigit x firstTime n
  | newNum < 10 = newNum
  | otherwise = superDigit newNum False n
  where newNum = if firstTime then n * func  else func
        func = digitSum $ show x


nTimesK n k =  read (concat $ replicate k (show n)) :: Integer


digitSum :: String -> Integer
digitSum xs = foldr (\x acc -> acc + toInteger (digitToInt x)) 0 xs
