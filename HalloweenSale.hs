readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

howManyGames :: Int -> Int -> Int -> Int -> Int
howManyGames p d m s 
  | s < p     = 0  
  | otherwise = 1 + howManyGames (max (p - d) m) d m (s - p) 

main = do
  [p, d, m, s] <- readInts
  print $ howManyGames p d m s

