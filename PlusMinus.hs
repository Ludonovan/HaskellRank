readFl :: IO [Float]
readFl = fmap (fmap read . words) getLine

--findP :: Ord a => [Int] -> Float
findP []     = 0
findP (x:xs) | x > 0     = (findP xs) + 1 
             | otherwise = findP xs

--findN :: Ord a => [Int] -> Float
findN []     = 0 
findN (x:xs) | x < 0     = (findN xs) + 1 
             | otherwise = findN xs

--findZ :: Eq a => [Int] -> Float
findZ []     = 0
findZ (x:xs) | x == 0    = (findZ xs) + 1 
             | otherwise = findZ xs

main = do
    n <- readLn :: IO Float
    arr <- readFl
    
    let pos = (findP arr) / n 
    let neg = (findN arr) / n
    let zer = (findZ arr) / n
    putStr $ show pos ++ "\n" ++ show neg ++ "\n" ++ show zer
