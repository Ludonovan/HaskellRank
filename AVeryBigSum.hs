readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

main = do
  n <- readLn :: IO Int
  ns <- readInts
  let ans = sum ns
  putStrLn (show ans)
