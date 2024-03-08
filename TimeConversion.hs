solve :: String -> String
solve t | t !! 8 == 'P' = solveP t
        | otherwise     = solveA t

solveP :: String -> String
solveP t | hour == "12" = "12" ++ drop 2 time
         | otherwise = show (read hour + 12) ++ drop 2 time 
  where hour = take 2 t 
        time = take 8 t

solveA :: String -> String
solveA t | hour == "12" = "00" ++ drop 2 time 
         | otherwise    = hour ++ drop 2 time
  where hour = take 2 t
        time =  take 8 t

main = do
  t <- getLine
  putStrLn $ solve t
