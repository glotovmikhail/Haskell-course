module Bench 
       ( nub'
       , nubA
       , main
       ) where

import           Criterion.Main
import           Criterion.Types
import           Data.List       (sort, nub)
import qualified Data.Set        as Set

nub' :: Ord a => [a] -> [a]
nub' = nub'' . sort
  where
    nub'' [] = []
    nub'' [a] = [a]
    nub'' (a : b : rest) = if a == b
                           then nub' (b : rest)
                           else a : nub' (b : rest)

nubA :: (Ord a) => [a] -> [a]
nubA = nubA' Set.empty
  where 
    nubA' _ [] = []
    nubA' s (x:xs) 
           | Set.member x s = nubA' s xs
           | otherwise    = x : nubA' (Set.insert x s) xs


main :: IO ()
main = defaultMainWith (defaultConfig {reportFile = Just "nub.html"
                                      , timeLimit = 30
                                      })
   [ bgroup "lenght = 25"
     [ bench "Lib" $ nf nub [1 :: Int, 1, 3, 3, 2, 6, 7, 3, 2, 5, 6, 3, 7, 8, 5, 4, 8, 9, 7, 7, 3, 6, 4, 8, 9]
     , bench "My" $ nf nub' [1 :: Int, 1, 3, 3, 2, 6, 7, 3, 2, 5, 6, 3, 7, 8, 5, 4, 8, 9, 7, 7, 3, 6, 4, 8, 9]
     , bench "Int" $ nf nubA [1 :: Int, 1, 3, 3, 2, 6, 7, 3, 2, 5, 6, 3, 7, 8, 5, 4, 8, 9, 7, 7, 3, 6, 4, 8, 9]
     ]
   ]