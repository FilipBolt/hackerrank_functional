import Control.Applicative
import Control.Monad
import System.IO
import Text.Printf
import Data.List.Split
import Data.List
import qualified Data.Map as Map

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    allPairs <- getMultipleLines n
    let seqPairs =  ((head allPairs), (last allPairs)) : zip allPairs (tail allPairs) 
    let res = Data.List.foldr (\(x, y) acc -> (calculateDistance x y) + acc) 0 seqPairs
    printf "%.1f" res
    return ()

getMultipleLines :: Int -> IO [(Int, Int)]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (parsePair x:xs)    
        return ret          

parsePair :: String -> (Int, Int)
parsePair xs = (read $ spl !! 0, read $ spl !! 1)
    where spl = splitOn " " xs

calculateDistance :: (Int, Int) -> (Int, Int) -> Float
calculateDistance (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))
