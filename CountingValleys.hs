countingValleys :: Int -> [Char] -> Int -> Int -> Int
countingValleys 0 _  c l   = c
countingValleys s (p:ps) c l
  | (l == 0) && (p == 'D') = countingValleys (s-1) ps (c+1) (l-1) 
  | (l /= 0) && (p == 'D') = countingValleys (s-1) ps c     (l-1)
  | otherwise              = countingValleys (s-1) ps c     (l+1)

main = do
  s <- readLn :: IO Int
  p <- getLine
  print $ countingValleys s p 0 0
