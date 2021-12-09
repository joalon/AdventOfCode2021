
import Test.Hspec

main :: IO ()
main = hspec $ do 
  describe "Test a truth" $ do
    it "should be true" $ do
      1 `shouldBe` 1
