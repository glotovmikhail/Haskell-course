module Block1 where

import           Data.List

order3 :: (Ord a) => (a, a, a) -> (a, a, a)
order3 (a, b, c)  = (x, y, z)
  where
    [x, y, z] = sort [a, b, c]

smartReplicate :: [Int] -> [Int]
smartReplicate []     = []
smartReplicate (x:xs) = replicate x x ++ smartReplicate xs

contains :: Int -> [[Int]] -> [[Int]]
contains a x = [y | y <- x, elem a y]

stringSum :: String -> Int
stringSum s = sum (map read (words s))