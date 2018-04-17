module Main where

import Test.Tasty (defaultMain, testGroup)

import Test.Unit (hspecTestTree)

main :: IO ()
main = hspecTestTree >>= \unitTests ->
       let allTests = testGroup "Expr" [unitTests]
       in defaultMain allTests