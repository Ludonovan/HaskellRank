readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

findW 0 _ _ (alice,bob)           = (alice,bob)
findW n (a:as) (b:bs) (alice,bob) = if a > b 
                                    then findW (n-1) as bs (alice+1,bob)
                                    else if b > a 
                                    then findW (n-1) as bs (alice,bob+1)
                                    else findW (n-1) as bs (alice,bob)

main = do
  a <- readInts
  b <- readInts
  
  let (al, bo) = (0,0)
  
  let (alice,bob) = findW 3 a b (al,bo)

  
  putStrLn $ show alice ++ " " ++ show bob
