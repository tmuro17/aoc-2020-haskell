import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day3" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input3.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 3a" $
                (solve3a strs)
                `shouldBe`
                191
            it "passes 3b" $
                (solve3b strs)
                `shouldBe`
                1478615040

solve3a :: [String] -> Integer
solve3a = treesOnSlope (3, 1) 0 . repeatList


solve3b :: [String] -> Integer
solve3b strs = product $ map (\s -> treesOnSlope s 0 $ repeatList strs) slopes

slopes :: [(Integer, Integer)]
slopes = [
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)]

treesOnSlope :: (Integer, Integer) -> Integer -> [String] -> Integer
treesOnSlope _ n [] = n
treesOnSlope (right, down) trees lst | hitTree lst = treesOnSlope (right, down) (trees + 1) transposed
                                     | otherwise = treesOnSlope (right, down) trees transposed
    where
        transposed = overList right $ downList down lst

hitTree :: [String] -> Bool
hitTree [] = False
hitTree lst = (=='#') $ head $ head lst

downList :: Integer -> [String] -> [String]
downList _ [] = []
downList 0 x = x
downList n x = downList (n - 1) $ tail x

overList :: Integer -> [String] -> [String]
overList _ [] = []
overList 0 x = x
overList n x = overList (n - 1) $ map tail x

repeatList :: [String] -> [String]
repeatList = map repeatString

repeatString :: String -> String
repeatString = foldr (++) "" . repeat
