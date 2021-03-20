import Test.Hspec

import ListOperations (len)

main :: IO ()
main = hspec $ do
  describe "ListOperations.len" $ do
    it "returns the number of elements in the list" $ do
      len [1..5] `shouldBe` 5
