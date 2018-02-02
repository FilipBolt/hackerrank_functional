import Data.List 

main :: IO()
main = do
    s <- getLine
    putStrLn $ nub s
    return ()
