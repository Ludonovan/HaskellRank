readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

solveMeFirst :: Int -> Int -> Int
solveMeFirst a b = a + b

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    let res = solveMeFirst a b
    print res

