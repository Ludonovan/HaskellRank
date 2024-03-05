readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

solve :: Int -> [Int] -> Int
solve 1 (a:b:c:d:e:[]) = b+c+d+e
solve 2 (a:b:c:d:e:[]) = a+c+d+e
solve 3 (a:b:c:d:e:[]) = a+b+d+e
solve 4 (a:b:c:d:e:[]) = a+b+c+e
solve 5 (a:b:c:d:e) = a+b+c+d 

findMin :: [Int] -> Int
findMin []     = 0
findMin [x]    = x
findMin (x:xs) | x < findMin xs = x
               | otherwise = findMin xs 

findMax :: [Int] -> Int
findMax []     = 0
findMax (x:xs) | x > findMax xs = x
               | otherwise = findMax xs

main = do
    arr <- readInts
    let a = solve 1 arr
    let b = solve 2 arr
    let c = solve 3 arr
    let d = solve 4 arr
    let e = solve 5 arr
    let min = findMin (a:b:c:d:e:[])
    let max = findMax (a:b:c:d:e:[])
    putStrLn $ show min ++ " " ++ show max
