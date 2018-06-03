module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.LensSpec  (hspecTestTree)

main :: IO()
main = do
        test1
        

test1 :: IO ()
test1 = hspecTestTree >>= \unitTests ->
          let allTests = testGroup "all tests" [unitTests]
          in defaultMain allTests