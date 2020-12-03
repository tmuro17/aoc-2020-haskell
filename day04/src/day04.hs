import qualified Data.Text as T
import qualified Data.Text.IO as TIO

--import Test.Tasty
--import Test.Tasty.Hspec

main :: IO ()
main = do
    text <- TIO.readFile "data/input4.txt"
    print text

-- Used to turn into Spec style later
{-
main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day4" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input4.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 4a" $
                True
                `shouldBe`
                True
            it "passes 4b" $
                False
                `shouldBe`
                False-}
