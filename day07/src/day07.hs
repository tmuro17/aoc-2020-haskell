import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad
import Test.Tasty
import Test.Tasty.Hspec
import qualified Data.Map as M

type Bag = String
data BagRule = Rule Bag [Content] deriving (Show, Eq, Ord)
data Content = Content Bag Int deriving (Show, Eq, Ord)

main :: IO ()
main = do
    text <- TIO.readFile "data/input7.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    ruleMap <- pure $ toRuleMap $ map toRule $ map words $ strs
    candidates <- pure $ filter (/= "shiny gold") $ map (\(Rule bag _) -> bag) $ map toRule $ map words $ strs
    print $ length $ filter (canContain ruleMap "shiny gold") $ candidates
    print $ countInnerBags ruleMap "shiny gold"

ex = ["shiny gold bags contain 2 dark red bags.",
      "dark red bags contain 2 dark orange bags.",
      "dark orange bags contain 2 dark yellow bags.",
      "dark yellow bags contain 2 dark green bags.",
      "dark green bags contain 2 dark blue bags.",
      "dark blue bags contain 2 dark violet bags.",
      "dark violet bags contain no other bags."]


type RuleMap = M.Map Bag [Content]

toRule :: [String] -> BagRule
toRule spec = Rule color contents
    where
        color = unwords $ take 2 spec
        contents = toContents $ drop 4 spec

toContents :: [String] -> [Content]
toContents ["bag."] = []
toContents ["bags."] = []
toContents ["no", "other", "bags."] = []
toContents (quantity : descriptor : color : xs) = Content (unwords [descriptor, color]) (read $ quantity :: Int) : toContents (drop 1 xs)
toContents _ = []

toRuleMap :: [BagRule] -> M.Map Bag [Content]
toRuleMap = M.fromList . map (\(Rule bag cnts) -> (bag, cnts))

canContain :: M.Map Bag [Content] -> Bag -> Bag -> Bool
canContain _ target search | target == search = True
canContain ruleMap target search = case M.lookup search ruleMap of
    Just contents ->  or $ map (\x -> canContain ruleMap target x) $ map (\(Content bag _) -> bag) $ contents
    Nothing -> False


countInnerBags :: RuleMap -> Bag -> Int
countInnerBags ruleMap key =  case M.lookup key ruleMap of
    Just [] -> 0
    Just contents -> sum $ map (\(Content bag num) -> max (num + num * countInnerBags ruleMap bag) num) contents
    Nothing ->  0


--main :: IO ()
--main = do
--    spc <- spec
--    tSpec <- testSpec "day6" spc
--    defaultMain (testGroup "Tests" [tSpec])
--
--spec :: IO (SpecWith ())
--spec = do
--    text <- TIO.readFile "data/input6.txt"
--    lns <- pure $ T.lines text
--    strs <- pure $ map T.unpack lns
--    return $ context "spec" $ do
--        describe "overall" $ do
--            it "passes 6a" $
--                solve6a strs
--                `shouldBe`
--                6686
--            it "passes 6b" $
--                solve6b strs
--                `shouldBe`
--                3476
