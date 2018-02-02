import Control.Applicative
import Control.Monad
import System.IO
import Text.Printf
import Data.List.Split
import qualified Data.Map as Map

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    allPairs <- getMultipleLines n
    putStrLn $ show $ getIntegral allPairs
    return ()

getMultipleLines :: Int -> IO [(Int, Int)]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine
        xs <- getMultipleLines (n - 1)
        let ret = (parsePair x:xs)
        return ret

parsePair :: String -> (Int, Int)
parsePair xs = (read $ spl !! 0 :: Int, read $ spl !! 1 :: Int)
    where spl = splitOn " " xs


getIntegral :: (Fractional a) => [(Int, Int)] -> a
getIntegral pairs = sumF
    where   xdata = map (fst) pairs
            ydata = map (snd) pairs
            sum1y = (tail ydata) ++ [head ydata]
            sum2x = (tail xdata) ++ [head xdata]
            sum1 = sum $ map (\(a, b) -> a * b) $ zip xdata sum1y
            sum2 = sum $ map (\(a, b) -> a * b) $ zip sum2x ydata
            sumN = abs $ sum1 - sum2
            sumF = (fromIntegral sumN) / 2
