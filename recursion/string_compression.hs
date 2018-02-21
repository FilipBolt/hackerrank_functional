import Data.List

main :: IO()
main = do
    str <- getLine
    putStrLn $ compress str
    return ()

transformGroups :: String -> String
transformGroups cs | length cs == 1 = [head cs]
                   | otherwise      = head cs : show (length cs)

compress :: String -> String
compress s = concatMap transformGroups $ group s
