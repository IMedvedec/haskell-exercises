import Test.Hspec

import FileAnalytics (
    Movie(..)
    ,highestGross
    ,highestIncome)

main :: IO ()
main = hspec $ do
    describe "FileAnalytics.highestGross" $ do
        it "returns Nothing for an empty movie list" $ do
            highestGross [] `shouldBe` (Nothing :: Maybe Movie)
        it "retruns a Just Movie with the highest gross in the given, non-empty list" $ do
            let movies = 
                    [
                    Movie {title="First movie",year=2020,gross=123000,budget=70000}
                    , Movie {title="Second movie",year=2019,gross=150000,budget=100000}
                    , Movie {title="Third movie",year=2018,gross=90000,budget=20000}
                    ]
            highestGross movies `shouldBe` (Just Movie {title="Second movie",year=2019,gross=150000,budget=100000} :: Maybe Movie)
    describe "FileAnalytics.highestIncome" $ do
        it "returns Nothing for an empty movie list" $ do
            highestIncome [] `shouldBe` (Nothing :: Maybe Movie)
        it "returns a Just Movie with the hightest income in the given, non-empty list" $ do
            let movies = 
                    [
                    Movie {title="First movie",year=2020,gross=123000,budget=70000}
                    , Movie {title="Second movie",year=2019,gross=150000,budget=100000}
                    , Movie {title="Third movie",year=2018,gross=90000,budget=20000}
                    ]
            highestIncome movies `shouldBe`(Just Movie {title="Third movie",year=2018,gross=90000,budget=20000} :: Maybe Movie)


