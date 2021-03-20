import Test.Hspec

import ListOperations (len,sumEven,toReverseDigits,toDigits)

main :: IO ()
main = hspec $ do
    describe "ListOperations.len" $ do
        it "returns the number of elements in the list" $ do
            len [1..5] `shouldBe` (5 :: Int)
        it "retruns zero for an empty list" $ do
            len [] `shouldBe` (0 :: Int)
    describe "ListOperations.sumEven" $ do
        it "returns the sum of elements on even indexes" $ do
            sumEven [1..3] `shouldBe` (4 :: Int)
        it "returns zero for an empty list" $ do
            sumEven [] `shouldBe` (0 :: Int)
    describe "ListOperations.toReverseDigits" $ do
        it "returns integer digits in reverse order" $ do
            toReverseDigits 123 `shouldBe` ([3,2,1] :: [Int])
        it "returns empty list for zero" $ do
            toReverseDigits 0 `shouldBe` ([] :: [Int])
    describe "ListOperations.toDigits" $ do
        it "returns integer digits in order" $ do
            toDigits 123 `shouldBe` ([1,2,3] :: [Int])
        it "returns empty list for zero" $ do
            toDigits 0 `shouldBe` ([] :: [Int])