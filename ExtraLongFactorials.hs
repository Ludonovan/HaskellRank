extraLongFactorials :: Integer -> Integer
extraLongFactorials 0 = 1
extraLongFactorials n = n * extraLongFactorials (n-1)

main = do
  n <- readLn :: IO Integer
  print $ extraLongFactorials n
