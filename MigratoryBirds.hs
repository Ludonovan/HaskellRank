import Data.List (sort, group)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

migB :: [[Int]] -> Int -> Int
migB []     _       = 0
migB (x:xs) m
    | length x == m = head x
    | otherwise     = migB xs m

main = do
    _     <- getLine
    arr   <- readInts
    let s = group (sort arr)
    let m = maximum (map length s)
    print $ migB s m

