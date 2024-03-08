import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

nextMult :: Int -> Int
nextMult n = ((n `div` 5) + 1) * 5

roundG :: Int -> Int
roundG n | n < 38                 = n
         | ((nextMult n) - n) < 3 = nextMult n
         | otherwise              = n


main = do
    n <- readLn :: IO Int
    nums <- replicateM n readInts
    let rounded = concatMap (map roundG) nums
    mapM_ print rounded
