import Terms
import TestingData
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "subt" $ do
    it "performs variable substitution" $ do
      subst "x" (Var "s") (Var "x") `shouldBe` (Var "s")
      subst "x" (Var "s") (Var "y") `shouldBe` (Var "y")
      
      
