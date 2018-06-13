import Data.List
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
    n_temp <- getLine
    x <- getLine
    let first = map (read :: String -> Int) (words x)
    m_temp <- getLine
    y <- getLine
    let second = map (read :: String -> Int) (words y)

    let firstF = Map.fromList $ createFreqMap first
    let secondF = Map.fromList $ createFreqMap second
    
    let result = map (fst) $ filter ((==True) . snd) $ diffMap secondF firstF
    putStrLn $ specialPrint $ result
    return ()


createFreqMap :: [Int] -> [(Int, Int)]
createFreqMap list = map (\x -> (head x, length x)) $ group $ sort list


diffMap :: Map.Map Int Int -> Map.Map Int Int -> [(Int, Bool)]
diffMap m1 m2 = map (\(el, freq) -> (el, isMissing freq (Map.lookup el m2))) (Map.toList m1)


isMissing :: Int -> Maybe Int -> Bool
isMissing freq1 mFreq2
  | isNothing mFreq2          = True
  | freq1 > (fromJust mFreq2) = True
  | otherwise                 = False


specialPrint :: [Int] -> String
specialPrint [] = ""
specialPrint (x:xs) = show x ++ " " ++ specialPrint xs
-- 
