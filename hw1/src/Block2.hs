module Block2 where

-- remove element

pop :: Int -> [a] -> Maybe a
pop i list = fst (remove i list)

remove :: Int -> [a] -> (Maybe a, [a])
remove _ []     = (Nothing, [])
remove 0 (x:xs) = (Just x, xs)
remove i (x:xs) = (fst rm, x : snd rm)
  where
    rm = remove (i - 1) xs

-- mergesort

merge :: [Int] -> [Int] -> [Int]
merge [] y          = y
merge x []          = x
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [a] = [a]
mergeSort x   = let half = div (length x) 2
                in
                    merge (mergeSort (take half x)) (mergeSort (drop half x))


