import Data.List 
import Data.Function


main :: IO()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    let result = y (n + 1)
    -- replicate to fill in the voids with '_' up to 63
    displayAll $ (replicate (63 - length result) []) ++ result
    return ()

divide :: (Num a) => [a] -> [a]
divide ones = concat $ map (\x -> [x - 1, x + 1]) ones

growSpread :: (Num a) => [a] -> [a]
growSpread ones = map (\(x, y) -> if even x then y - 1 else y + 1 ) (zip [0..] ones)

createLine :: (Num a, Eq a, Enum a) => [a] -> String
createLine ones = map (\x -> if x `elem` ones then '1' else '_') [0..99]

displayAll :: (Enum a, Eq a, Num a) => [[a]] -> IO ()
displayAll x = putStr $ unlines $ map createLine x

reverseConcat :: [a] -> [[a]]
reverseConcat = map (:[]) 

-- constantLineIndex is the entry point which we replicate num times
-- we split the last element of the entry point and 
-- iteratively apply growSpread which does the divergence
-- accumulate current result and add new line + divergence
total (resultAcc, num) = ((reverse (take num (iterate growSpread oneToSplit))) ++ core, num `div` 2)
  where core = replicate num constantLineIndex
        oneToSplit = divide (head core)
        constantLineIndex = head resultAcc

-- this method starts at index 49 and line length of 16
-- then calls it self with the result up to infinity
recurse :: [([[Integer]], Int)]
recurse = iterate total ([[49]], 16)


y :: Int -> [[Integer]]
y n = concat $ map fst $ reverse $ tail $ take n recurse
