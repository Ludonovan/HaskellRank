fw :: Int -> Int -> Int
fw p n 
  | (p `mod` 2 == 0) = p `div` 2
  | otherwise        = (p-1) `div` 2

bw :: Int -> Int -> Int
bw p n 
  | (p `mod` 2 == 0) && (n `mod` 2 == 0) = (n - p) `div` 2
  | (p `mod` 2 == 0) && (n `mod` 2 == 1) = ((n-1) - p) `div` 2
  | (p `mod` 2 == 1) && (n `mod` 2 == 0) = (n - (p+1)) `div` 2
  | (p `mod` 2 == 1) && (n `mod` 2 == 1) = ((n-1)-(p-1)) `div` 2
  | otherwise = -1000

pageCount :: Int -> Int -> Int
pageCount n p 
  | (p == n) || (p == 1) || ((p `mod` 2 == 0) && (p == n-1)) = 0
  | ((p `mod` 2 == 1) && (p == n-1)) || 
    ((p `mod` 2 == 0) && (p == n+1))    = 1
  | fw p n > bw p n = bw p n 
  | otherwise       = fw p n 


main = do
  n <- readLn :: IO Int
  p <- readLn :: IO Int
  print $ pageCount n p
