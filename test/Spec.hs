import Test.QuickCheck
import Test.Hspec
import Tensor

main :: IO ()
main = prop_mult

prop_mult = hspec $ do
    describe "Tensor Multiply" $ do
        it "t2 multiply by t3 is t4" $ do
            t2 * t3 `shouldBe` t4
