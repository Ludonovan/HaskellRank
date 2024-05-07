import Data.List (sort, group)

equalizeArray :: [Int] -> Int
equalizeArray a = length a - maximum (map length (group (sort a)))

main = do
    _ <- getLine
    a <- fmap (fmap read . words) getLine
    print $ equalizeArray a
