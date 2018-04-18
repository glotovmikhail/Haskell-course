module Test.Unit 
       ( hspecTestTree,
         hspecTestStrSum
       ) where

import Block1 (IntExpr (..)
              , evalInt)
import Block2 (stringSum)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec, context)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "ArithmeticExpr" arithmetic_Expr

hspecTestStrSum :: IO TestTree
hspecTestStrSum = testSpec "StingSum" stringSum_test

arithmetic_Expr :: Spec
arithmetic_Expr = do
    describe "check expressions" $
        context "there a = 1, b = 2, c = 3, d = 4, e = 12" $ do
          let [a', b', c', d', e'] = [1 :: Int, 2, 3, 4, 12] 
          let [a, b, c, d, e] = map IntConst [a', b', c', d', e']
          it "(b + b) / (d * a)" $ do
              let result = (b' + b') `div` (d' * a')
              evalInt (IntDiv (IntSum b b) (IntMul d a)) `shouldBe` Right result
          it "((c * d) / e) / (c - b)" $ do
              let result = ((c' * d') `div` e') `div` (c' - b')
              evalInt (IntDiv (IntDiv (IntMul c d) e) (IntSub c b)) `shouldBe` Right result
          it "((c ^ b) + d) - e" $ do
              let result = ((c' ^ b') + d' - e')
              evalInt (IntSub (IntSum (IntPow c b) d) e) `shouldBe` Right result

    describe "check div" $
        context "there a = 0, b = -1, c = 3, d = 4, e = 12" $ do
          let [a', b', c', d', e'] = [0 :: Int, 1, 3, 4, 12]
          let [a, b, c, d, e] = map IntConst [a', b', c', d', e']
          it "standart integral division -- (e / d)" $
              evalInt (IntDiv e d) `shouldBe` evalInt c
          it "division by zero -- (c / a)" $
              evalInt (IntDiv c a) `shouldBe` Left "Division by zero"
          it "division by negative -- (c / b)" $
              evalInt (IntDiv c b) `shouldBe` Right (c' `div` b')
          it "nonintegral division -- (e / (c + d))" $
              evalInt (IntDiv e (IntSum c d)) `shouldBe` Right (e' `div` (c' + d'))
          it "negative devision -- (b / b)" $
              evalInt (IntDiv b b) `shouldBe` Right (b' `div` b')

    describe "check pow" $
        context "there a = -1, b = 0, c = 2, d = 3" $ do
          let [a', b', c', d'] = [-1 :: Int, 0, 2, 3]
          let [a, b, c, d] = map IntConst [a', b', c', d']
          it "standart pow -- (c ^ d)" $
              evalInt (IntPow c d) `shouldBe` Right (c' ^ d')
          it "negative base pow -- (a ^ d)" $
              evalInt (IntPow a d) `shouldBe` Right (a' ^ d')
          it "negative base pow -- (a ^ c)" $
              evalInt (IntPow a c) `shouldBe` Right (a' ^ c')
          it "zero pow -- (d ^ b)" $
              evalInt (IntPow d b) `shouldBe` Right (d' ^ b')
          it "negative pow -- (c ^ a)" $
              evalInt (IntPow c a) `shouldBe` Left "Negative arg in exponent"


stringSum_test :: Spec
stringSum_test = do
    describe "good tests" $
        context "only valid input" $ do
          let s1 = "\n1\t\n3   555  -1\n\n\n-5"
          let s2 = "\t-12345\t"
          let s3 = "123\t\n\t\n\t\n321 -4 -40"
          let stringSumCor s = sum (map read (words s))
          it "\"\n1\t\n3   555  -1\n\n\n-5\"" $
              stringSum s1 `shouldBe` Just (stringSumCor s1)
          it "\"\t-12345\t\"" $
              stringSum s2 `shouldBe` Just (stringSumCor s2)
          it "\"123\t\n\t\n\t\n321 -4 -40\"" $
              stringSum s3 `shouldBe` Just (stringSumCor s3)
    describe "mustfail tests" $
        context "only invalid input" $ do
          let s1 = "asd"
          let s2 = "1-1"
          let s3 = "1.2"
          let s4 = "--2"
          let s5 = "+1"
          let s6 = "1+"
          it "\"asd\"" $
              stringSum s1 `shouldBe` Nothing
          it "\"1-1\"" $
              stringSum s2 `shouldBe` Nothing
          it "\"1.2\"" $
              stringSum s3 `shouldBe` Nothing
          it "\"--2\"" $
              stringSum s4 `shouldBe` Nothing
          it "\"+1\"" $
              stringSum s5 `shouldBe` Nothing
          it "\"1+\"" $
              stringSum s6 `shouldBe` Nothing

