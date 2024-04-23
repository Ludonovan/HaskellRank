import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

angryProfessor :: Int -> [Int] -> [Char]
angryProfessor k xs 
  | length (filter (<=0) xs) >= k = "NO"
  | otherwise                    = "YES" 
  
main = do
  numTests <- readLn :: IO Int
  ns       <- replicateM numTests $ do
    [n, k]   <- readInts
    arrivals <- readInts
    putStrLn $ angryProfessor k arrivals
  return 0
