import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    tSpec <- testSpec "spec" spec
    defaultMain (testGroup "Tests" [tSpec])

spec = context "day1" $ do
    it "passes simple test" $
        True == True

