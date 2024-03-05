readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
    _ <- getLine
    arr <- readInts
    let r = sum arr
    print r
    
