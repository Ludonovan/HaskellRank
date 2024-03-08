import Control.Monad (replicateM)

readInts :: IO [Int]
readInts = fmap (fmap read . words) getLine

findPrim :: [[Int]] -> Int -> [Int]
findPrim [] _     = []
findPrim (x:xs) n = (x !! (n - length xs - 1)) : findPrim xs n

findSec :: [[Int]] -> Int -> [Int]
findSec [] _     = []
findSec (x:xs) n = (x !! (length xs)) : findSec xs n

diagonalDifference :: [Int] -> [Int] -> Int
diagonalDifference [] _  = 0
diagonalDifference _ []  = 0
diagonalDifference xs ys = abs $ sum xs - sum ys

main :: IO ()
main = do
  n   <- readLn :: IO Int
  arr <- replicateM n readInts
  let prim = findPrim arr n
  let sec  = findSec arr n
  let ans  = diagonalDifference prim sec
  putStrLn $ show ans
