import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Data.Map as M
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
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 2a" $
                (length $ filter (\[range, tc, pass] -> isValid (rangeToTuple range) (targetChar tc) (T.unpack pass)) $ map (T.words) lns)
                `shouldBe`
                416
            it "passes 2b" $
                (length $ filter (\[range, tc, pass] -> isValid' (rangeToTuple range) (targetChar tc) (T.unpack pass)) $ map (T.words) lns)
                `shouldBe`
                 688

rangeToTuple :: T.Text -> (Integer, Integer)
rangeToTuple txt = (\[l, u] -> (read $ T.unpack l, read $ T.unpack u)) $ T.splitOn (T.pack "-") txt

targetChar :: T.Text -> Char
targetChar = head . T.unpack

isValid :: (Integer, Integer) -> Char -> String -> Bool
isValid (lBnd,uBnd) target pass = case M.lookup target $ M.fromListWith (+) [(c, 1) | c <- pass] of
    Just x -> (x >= lBnd) && (x <= uBnd)
    Nothing -> False

isValid' :: (Integer, Integer) -> Char -> String -> Bool
isValid' (pos1, pos2) target pass = ((pos1 `elem` psns) && (not $ pos2 `elem` psns)) || ((not $ pos1 `elem` psns) && (pos2 `elem` psns))
    where
        psns = map fst $ filter ((== target) . snd) $ zip [1..] pass
