import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad
import Test.Tasty
import Test.Tasty.Hspec
import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day6" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input6.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 6a" $
                solve6a strs
                `shouldBe`
                6686
            it "passes 6b" $
                solve6b strs
                `shouldBe`
                3476

solve6a :: [String] -> Int
solve6a = sum . map length . map (nub . join) . splitOn [""]

solve6b :: [String] -> Int
solve6b = sum . map groupAllAnswered . splitOn [""]

groupAllAnswered :: [String] -> Int
groupAllAnswered = length . ap (filter . (. snd) . (==) . length) (M.toList . M.fromListWith (+) . map (flip (,) 1) . join)
