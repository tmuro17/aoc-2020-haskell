import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Map as M

import Data.List.Split
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day2" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input2.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 2a" $
                solve2a strs
                `shouldBe`
                416
            it "passes 2b" $
                solve2b strs
                `shouldBe`
                 688
solve2a :: [String] -> Int
solve2a = length . filter (\[range, tc, pass] -> isValid (rangeToTuple range) (head tc) pass) . map words

solve2b :: [String] -> Int
solve2b = length . filter (\[range, tc, pass] -> isValid' (rangeToTuple range) (head tc) pass) . map words

rangeToTuple :: String -> (Integer, Integer)
rangeToTuple = (\[l, u] -> (read l, read u)) . splitOn "-"


isValid :: (Integer, Integer) -> Char -> String -> Bool
isValid (lBnd,uBnd) target pass = case M.lookup target $ M.fromListWith (+) [(c, 1) | c <- pass] of
    Just x -> (x >= lBnd) && (x <= uBnd)
    Nothing -> False

isValid' :: (Integer, Integer) -> Char -> String -> Bool
isValid' (pos1, pos2) target pass = ((pos1 `elem` psns) && (not $ pos2 `elem` psns)) || ((not $ pos1 `elem` psns) && (pos2 `elem` psns))
    where
        psns = map fst $ filter ((== target) . snd) $ zip [1..] pass
