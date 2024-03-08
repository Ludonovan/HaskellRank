space :: Int -> String
space 0 = ""
space n = " " ++ space (n-1)

symbol :: Int -> String
symbol 0 = ""
symbol n = "#" ++ symbol (n-1)

staircase :: Int -> Int -> String
staircase n m | n > m = ""
              | otherwise = space (m-n) ++ symbol n ++ "\n" 
                ++ staircase (n+1) m


main = do
    n <- readLn :: IO Int
    putStr $ staircase 1 n

