import Test.Hspec

import HelloWorld (hello)

main :: IO ()
main = hspec $ do
  describe "HelloWorld.hello" $ do
    it "returns \"Hello world!\" string" $ do
      hello `shouldBe` "Hello world!"
