module Test.LensSpec
       ( spec
       , hspecTestTree
       ) where

import Lens
import Test.Tasty       (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec, context)

hspecTestTree :: IO TestTree
hspecTestTree = testSpec "All Tests" spec

spec :: Spec
spec = describe "Lens (String, Int)" $ do
    let w1 = "justword"
    let w2 = "another"
    let w3 = 239 :: Int
    let w4 = 30  :: Int
    let pair = (w2, w3)
    context "check set" $ do
        it "_1" $
            set _1 w1 pair `shouldBe` (w1, w3)
        it "_2" $
            set _2 w4 pair `shouldBe` (w2, w4)
    context "check view" $ do
        it "_1" $
            view _1 pair `shouldBe` w2
        it "_2" $
            view _2 pair `shouldBe` w3
    context "check over" $ do
        it "_1 (const another)" $
            over _1 (const w2) pair `shouldBe` (w2, w3)
        it "_2 (++)" $
            over _2 (+1) pair `shouldBe` (w1, 240)
