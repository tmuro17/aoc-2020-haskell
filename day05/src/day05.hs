import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day5" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input5.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 5a" $
                solve5a strs
                `shouldBe`
                930
            it "passes 5b" $
                solve5b strs
                `shouldBe`
                515

solve5a :: [String] -> Int
solve5a = maximum . map seatID

solve5b :: [String] -> Int
solve5b = head . ap (filter . flip plusMinus . map seatID) missing

plusMinus :: Int -> [Int] -> Bool
plusMinus = ap (ap . ((&&) .) . elem . (1 +)) (elem . subtract 1)

missing :: [String] -> [Int]
missing = flip filter allSeats . (not .) . flip elem . map seatID

allSeats :: [Int]
allSeats = [x * 8 + y | x <- [0..127], y <- [0..7]]

seatID :: String -> Int
seatID = ap ((+) . (8 *) . rowNum . take 7) (colNum . drop 7)

rowNum :: String -> Int
rowNum = sum . map fst . filter ((=='B') . snd) . zip [2 ^ x | x <- [6,5..0]]

colNum :: String -> Int
colNum = sum . map fst . filter ((=='R') . snd) . zip [2 ^ x | x <- [2,1,0]]


