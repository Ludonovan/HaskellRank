import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

catAndMouse :: Int -> Int -> Int -> [Char]
catAndMouse x y z 
  | abs(z-x) < abs(y-z) = "Cat A"
  | abs(z-x) > abs(y-z) = "Cat B"
  | otherwise     = "Mouse C"

main = do
  q   <- readLn :: IO Int
  arr <- replicateM q $ do
    [a,b,c] <- readInts
    putStrLn $ catAndMouse a b c
  return 0
