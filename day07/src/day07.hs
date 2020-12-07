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
    spc <- spec
    tSpec <- testSpec "day7" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input7.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 7a" $
                solve7a strs
                `shouldBe`
                226
            it "passes 7b" $
                solve7b strs
                `shouldBe`
                9569

type RuleMap = M.Map Bag [Content]

solve7a = length . ap (filter . flip canContain "shiny gold" . toRuleMap . map (toRule . words)) (filter ("shiny gold" /=) . map (toBag . words))
--solve7a strs = length $ filter (canContain ruleMap "shiny gold") $ candidates
--    where
--        ruleMap = toRuleMap $ map toRule $ map words $ strs
--        candidates = filter (/= "shiny gold") $ map toBag $ map toRule $ map words $ strs


solve7b = flip countInnerBags "shiny gold" . toRuleMap . map (toRule . words)

--solve7b strs = countInnerBags ruleMap "shiny gold"
--    where ruleMap = toRuleMap $ map toRule $ words strs

toBag :: [String] -> Bag
toBag = unwords . take 2

toRule :: [String] -> BagRule
toRule = liftM2 Rule toBag (toContents . drop 4)
--toRule specs = Rule color contents
--    where
--        color = toBag specs
--        contents = toContents $ drop 4 specs

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
    Nothing -> False
    Just contents ->  or $ map (canContain ruleMap target) $ map (\(Content bag _) -> bag) $ contents


countInnerBags :: RuleMap -> Bag -> Int
countInnerBags ruleMap key =  case M.lookup key ruleMap of
    Just [] -> 0
    Nothing ->  0
    Just contents -> sum $ map (\(Content bag num) -> max (num + num * countInnerBags ruleMap bag) num) contents


