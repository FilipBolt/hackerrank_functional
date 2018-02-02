main :: IO ()
main = do
    s1 <- getLine
    s2 <- getLine
    let prefix = getPrefix s1 s2
    putStrLn $ show (length prefix) ++ " " ++ prefix
    let s1Res = getRes s1 prefix
    let s2Res = getRes s2 prefix
    putStrLn $ show (length s1Res) ++ " " ++ s1Res
    putStrLn $ show (length s2Res) ++ " " ++ s2Res
    return ()


getPrefix :: String -> String -> String
getPrefix s1 s2 = reverse $ snd $ foldl f (True, "") (zip s1 s2)

f :: (Bool, String) -> (Char, Char) -> (Bool, String)
f (prevPrefix, accumPrefix) (x, y) 
    | x == y && prevPrefix == True = (True, x : accumPrefix)
    | otherwise                    = (False, accumPrefix)

getRes :: String -> String -> String
getRes s1 prefix = reverse $ take (length s1 - length prefix) (reverse s1)
