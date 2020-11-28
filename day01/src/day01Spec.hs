import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    tSpec <- testSpec "day1" spec
    defaultMain (testGroup "Tests" [tSpec])

spec = context "spec" $ do
    it "passes simple test" $
        True == True

