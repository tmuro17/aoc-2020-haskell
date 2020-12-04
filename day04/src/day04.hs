import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List.Split (splitOn)
--import Test.Tasty
--import Test.Tasty.Hspec

main :: IO ()
main = do
    text <- TIO.readFile "data/input4.txt"
    lines <- pure $ T.lines text
    strs <- pure $ map T.unpack lines
    split <- pure $ splitOn [""] strs
    oneLines <- pure $ map unwords split
    detailList <- pure $ map words oneLines
    keyValTuples <- pure $ map (map keyPairTuple) detailList
    keyLists <- pure $ map (map fst) keyValTuples
    print $ length $ filter hasRequired keyLists
    print $ length $ filter (all isValidEntry) $ filter (\x -> hasRequired (map fst x)) keyValTuples


keyPairTuple :: String -> (String, String)
keyPairTuple entry = (head splits, last splits)
    where
        splits = splitOn ":" entry

hasRequired :: [String] -> Bool
hasRequired keys = all (`elem` keys) required
    where
        required :: [String]
        required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


isValidEntry :: (String, String) -> Bool
isValidEntry ("byr", yr) = (yNum >= 1920 && yNum <= 2002)
    where yNum = read yr :: Int
isValidEntry ("iyr", yr) = (yNum >= 2010 && yNum <= 2020)
    where yNum = read yr :: Int
isValidEntry ("eyr", yr) = (yNum >= 2020 && yNum <= 2030)
    where yNum = read yr :: Int
isValidEntry ("ecl", color) = color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidEntry ("hgt", hgt) | last hgt == 'm' = processCm num
                          | last hgt == 'n' = processIn num
                          | otherwise = False
    where
        num = read (init $ init hgt)
        processCm n = (n >= 150 && n <= 193)
        processIn n = (n >= 59 && n <= 76)
isValidEntry ("hcl", color) = length color == 7 && isColor (tail color)
    where isColor c = all (\x -> (x `elem` ['0'..'9']) || (x `elem` ['a'..'f'])) c
isValidEntry ("pid", num) = length num == 9 && all (`elem` ['0'..'9']) num
isValidEntry ("cid", _) = True

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
