readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine


numTallest :: [Int] -> Int -> Int
numTallest _ 0      = 0
numTallest [] _     = 0
numTallest (x:xs) n | x == n    = 1 + numTallest xs n
                    | otherwise = numTallest xs n

main = do
  _ <- getLine
  arr <- readInts
  print $ numTallest arr (maximum arr)
