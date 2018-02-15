import qualified Data.Map as Map
import qualified Data.List as List

main :: IO ()
main = do
    _ <- getLine
    a_temp <- getLine
    let a = map (read :: String -> Integer) (words a_temp)
    let factoredA = List.foldr (\x acc -> primeFactors x ++ acc) [] a
    let dictA = count factoredA
    let factorsA = Map.keys dictA

    _ <- getLine
    b_temp <- getLine
    let b = map (read :: String -> Integer) (words b_temp)
    let factoredB = List.foldr (\x acc -> primeFactors x ++ acc) [] b
    let dictB = count factoredB
    let factorsB = Map.keys dictB

    let allFactors = List.union factorsA factorsB

    let minFactors = map (\x -> (x, getMinFactor dictA dictB x)) allFactors

    let factToNum = foldr (\x acc -> if snd x == Nothing then acc else acc * (fst x) ^ ((fromJust . snd) x)) 1 minFactors

    putStrLn $ show (factToNum `mod` 1000000007)
    return ()

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors n
    | factors == []  = [n]
    | otherwise = factors ++ primeFactors (n `div` (head factors))
    where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]


count :: (Ord a, Integral b) => [a] -> Map.Map a b
count = List.foldr updateMap Map.empty
            where updateMap v counts
                    | Map.member v counts = Map.adjust succ v counts
                    | otherwise           = Map.insert v 1 counts

getMinFactor :: Map.Map Integer Integer -> Map.Map Integer Integer -> Integer -> Maybe Integer
getMinFactor a b fact = min (Map.lookup fact a) (Map.lookup fact b)

fromJust :: Maybe a -> a
fromJust Nothing = _
fromJust (Just x) = x
