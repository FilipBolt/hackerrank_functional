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
    -- print $ show allPairs
    let res = map (\x -> if isFunction x then "YES" else "NO") allPairs
    printList res
    return ()
 
getMultipleLines :: Int -> IO [[(Int, Int)]]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine
        functionSamples <- replicateM (read x) getLine 
        let fInts = map parsePair functionSamples
        xs <- getMultipleLines (n - 1)
        let ret = (fInts:xs)
        return ret


parsePair :: String -> (Int, Int)
parsePair xs = (read $ spl !! 0, read $ spl !! 1)
    where spl = splitOn " " xs


-- something is a function if it has same different out for different in
isFunction :: [(Int, Int)] -> Bool
-- create a map than compare it's size with the list
isFunction x = length (Map.fromList x) == length x


printList :: [String] -> IO ()
printList [] = return ()
printList (x:xs) = do
    putStrLn x
    printList xs
    return ()

