readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

count :: Int -> Int -> Int -> [Int] -> Int
count _ _ _ []     = 0
count s t a (x:xs) 
  | a+x >= s && a+x <= t = 1 + count s t a xs
  | otherwise = count s t a xs
  
main = do
  [s,t]  <- readInts
  [a,b]  <- readInts
  [m,n]  <- readInts
  distsA <- readInts
  distsB <- readInts
  let apples  = count s t a distsA
  let oranges = count s t b distsB 
  print apples
  print orange
