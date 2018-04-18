module Main where

import Hedgehog (check)
import Test.Tasty (defaultMain, testGroup)

import Test.Unit (hspecTestTree, hspecTestStrSum)
import Test.Prop (prop_bin)

main :: IO()
main = do
        test1'2
        test1'1

test1'1 :: IO ()
test1'1 = hspecTestTree >>= \unitTests ->
          let allTests = testGroup "Expr" [unitTests]
          in defaultMain allTests

test1'2 :: IO ()
test1'2 = hspecTestStrSum >>= \unitTests ->
          let allTests = testGroup "StrSum" [unitTests]
          in defaultMain allTests

test2 :: IO Bool
test2 = check prop_bin