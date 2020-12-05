import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Someone's library so that I can split lists easily
import Data.List.Split (splitOn)
import Control.Monad
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    text <- TIO.readFile "data/input5.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    print $ maximum $ map seatID $ strs
    there <- pure $ map seatID strs
    print $ filter (flip plusMinus there) $ (missing strs)

plusMinus seat there = (seat + 1 `elem` there) && (seat - 1 `elem` there)

missing strs = (\there possible -> filter (not . (`elem` there)) possible) (map seatID strs) allSeats

allSeats = [x * 8 + y | x <- [0..127], y <- [0..7]]

seatID :: String -> Int
seatID = ap ((+) . (8 *) . rowNum . take 7) (colNum . drop 7)

rowNum :: String -> Int
rowNum = sum . map fst . filter ((=='B') . snd) . zip [2 ^ x | x <- [6,5..0]]

colNum :: String -> Int
colNum = sum . map fst . filter ((=='R') . snd) . zip [2 ^ x | x <- [2,1,0]]

--main :: IO ()
--main = do
--    spc <- spec
--    tSpec <- testSpec "day4" spc
--    defaultMain (testGroup "Tests" [tSpec])
--
--spec :: IO (SpecWith ())
--spec = do
--    text <- TIO.readFile "data/input4.txt"
--    lns <- pure $ T.lines text
--    strs <- pure $ map T.unpack lns
--    return $ context "spec" $ do
--        describe "overall" $ do
--            it "passes 4a" $
--                solve4a strs
--                `shouldBe`
--                208
--            it "passes 4b" $
--                solve4b strs
--                `shouldBe`
--                167
