import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Someone's library so that I can split lists easily
import Data.List.Split (splitOn)
import Control.Monad
import Test.Tasty
import Test.Tasty.Hspec

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
                solve4a strs
                `shouldBe`
                208
            it "passes 4b" $
                solve4b strs
                `shouldBe`
                167


--Fixme: Provide some clarity to function compositions, or make some of them functions
solve4a :: [String] -> Int
solve4a = length . filter hasRequired . map (map fst) . map (map keyPairTuple) . toEntryLists . splitOn [""]

solve4b :: [String] -> Int
solve4b = length . filter (all isValidEntry) . filter (hasRequired . map fst) . map (map keyPairTuple) . toEntryLists . splitOn [""]

toEntryLists :: [[String]] -> [[String]]
toEntryLists = map words . map unwords

keyPairTuple :: String -> (String, String)
keyPairTuple entry = (head splits, last splits)
    where
        splits = splitOn ":" entry

hasRequired :: [String] -> Bool
hasRequired = flip all required . flip elem
    where
        required :: [String]
        required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


isValidEntry :: (String, String) -> Bool
isValidEntry ("byr", yr) = boundsCheck 1920 2002 $ yNum
    where yNum = read yr :: Int
isValidEntry ("iyr", yr) = boundsCheck 2010 2020 $ yNum
    where yNum = read yr :: Int
isValidEntry ("eyr", yr) = boundsCheck 2020 2030 $ yNum
    where yNum = read yr :: Int
isValidEntry ("ecl", color) = color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidEntry ("hgt", hgt) | last hgt == 'm' = processCm num
                          | last hgt == 'n' = processIn num
                          | otherwise = False
    where
        num = read $ init $ init hgt :: Int
        processCm = boundsCheck 150 193
        processIn = boundsCheck 59 76
isValidEntry ("hcl", color) = length color == 7 && isColor (tail color)
    where isColor = all (liftM2 (||) (`elem` ['0'..'9']) (`elem` ['a'..'f']))
isValidEntry ("pid", num) = length num == 9 && all (`elem` ['0'..'9']) num
isValidEntry ("cid", _) = True
isValidEntry _ = False

boundsCheck :: Int -> Int -> Int -> Bool
boundsCheck lower upper = liftM2 (&&) (>= lower) (<= upper)
