import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.Hspec
import Control.Monad

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day9" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input9.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 9a" $
                solve9a strs
                `shouldBe`
                Just 18272118
            it "passes 9b" $
                solve9b strs
                `shouldBe`
                Just 2186361

solve9a :: [String] -> Maybe Int
solve9a = ap (checkList . take 25 . map read) (drop 25 . map read)
--solve9a strs = checkList (take 25 ints) (drop 25 ints)
--    where ints = map read strs

solve9b :: [String] -> Maybe Int
solve9b = flip (crackEncryption 2 . map read) 18272118
--solve9b strs = crackEncryption 2 ints 18272118
--    where ints = map read strs

checkList :: [Int] -> [Int] -> Maybe Int
checkList [] _ = Nothing
checkList _ [] = Nothing
checkList preamb nums | not $ isSumOfTwo (head nums) preamb [] = Just $ head nums
                      | otherwise = checkList (drop 1 preamb ++ [head nums]) (drop 1 nums)


isSumOfTwo :: Int -> [Int] -> [Int] -> Bool
isSumOfTwo _ [] _ = False
isSumOfTwo n (x:xs) seen | (n - x) `elem` xs || (n - x) `elem` seen = True
                         | otherwise = isSumOfTwo n xs (x : seen)

crackEncryption :: Int -> [Int] -> Int -> Maybe Int
crackEncryption _ [] _ = Nothing
crackEncryption testing nums target | testSum == target = Just $ (minimum $ take testing nums) + (maximum $ take testing nums)
                                    | testSum > target = crackEncryption 2 (tail nums) target
                                    | (testing + 1) > length nums = Nothing
                                    | testSum < target = crackEncryption (testing + 1) nums target
                                    | otherwise = Nothing
    where
        testSum = sum $ take testing nums


