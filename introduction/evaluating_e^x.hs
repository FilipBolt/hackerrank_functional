import Control.Applicative
import Control.Monad
import System.IO
import Text.Printf


evaluate x a = sum [ x ^ index * coef  | (index, coef) <- zip [0..]  a ]

factorial :: Double -> Double

-- first defining factorial(0) = 1 so we have the recursive end condition
factorial 0 = 1
-- we define the formula of the factorial
factorial n = n * factorial (n - 1)

-- Maclaurin series
-- function has prededence over division
maclaurin = [ 1 / factorial x | x <- [0..] ]

-- exp function for first 10 elements, we make the maclaurin series 
-- the coefficient for the polynomial series sum
exp' x = evaluate x $ take 10 maclaurin 

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    lns <- getMultipleLines n
    let nums = [exp' (read x:: Double) | x <- lns]
    mapM_ (printf "%.4f\n") nums
    return ()
    
 
getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

