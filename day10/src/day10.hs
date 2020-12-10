import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.Hspec
import Data.List


main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day10" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input10.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 10a" $
                solve10a strs
                `shouldBe`
                1856
            it "passes 10b" $
                solve10b strs
                `shouldBe`
                2314037239808

-- I see now that there are better ways to do this, but this is how I did it
solve10a :: [String] -> Int
solve10a strs = (\(x, y) -> x * y) $ countJoltsDiffs sorted 0 (0, 0)
    where
        ints :: [Int]
        ints = map read strs
        sorted = sort ints


solve10b :: [String] -> Int
solve10b strs = countArranges sorted
    where
        ints :: [Int]
        ints = map read strs
        sorted = sort ints

-- Expects a sorted list as input
countJoltsDiffs :: [Int] -> Int -> (Int, Int) -> (Int, Int)
countJoltsDiffs [] _ (ones, threes) = (ones, succ threes)
countJoltsDiffs adapts start (ones, threes) | (next - start) == 1 = countJoltsDiffs (tail adapts) next (succ ones, threes)
                                            | (next - start) == 2 = countJoltsDiffs (tail adapts) next (ones, threes)
                                            | (next - start) == 3 = countJoltsDiffs (tail adapts) next (ones, succ threes)
                                            | otherwise = (ones, threes)
    where next = head adapts

-- Credit to https://www.reddit.com/user/Zealousideal-Track82/ as I could not really figure this one out.
-- Using it as a jumping off point for refresher into DP as well as DP in Haskell
-- I wanted to understand this one before pushing,
countArranges :: [Int] -> Int
countArranges nums = product $ (map ((dp !!) . length)) $ filter ((== 1) . head) $ group diffs
    where
        list = 0 : nums ++ [maximum nums + 3]
        diffs = zipWith (-) (tail list) list
        dp = 1 : 1 : 2 : zipWith3 (\x y z -> x + y + z) dp (drop 1 dp) (drop 2 dp)
