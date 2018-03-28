main :: IO()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    result <- readLines t
    -- first I'll recreate the tree
    -- then I'll do preorder of tree
    -- in a list and compare to original list

    putStr $ unlines result
    return ()


readLines :: Int -> IO [String]
readLines n
    | n <= 0 = return []
    | otherwise = do
        _ <- getLine
        x <- getLine
        let nums = map read $ words x :: [Int]
        xs <- readLines (n - 1)
        let same = (preorderTreeToList $ listToTree nums) == nums
        return (boolToString same : xs)

data Tree a = Null | Node a (Tree a) (Tree a)
  deriving (Show, Eq, Ord)

boolToString :: Bool -> String
boolToString bool
  | bool      = "YES"
  | otherwise = "NO"

listToTree :: (Ord a) => [a] -> Tree a
listToTree xs = foldr (\x acc -> insertTree x acc) Null (reverse xs)

preorderTreeToList ::  Tree a -> [a]
preorderTreeToList Null = []
preorderTreeToList (Node a left right) = [a] ++ (preorderTreeToList left) ++ (preorderTreeToList right)

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree element Null = Node element (Null) (Null)
insertTree element (Node a left right)
  | element < a = Node a (insertTree element left) right
  | element > a = Node a left (insertTree element right)
