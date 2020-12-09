import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.Hspec
import qualified Data.Map as M


data Op = NOP | JMP | ACC deriving (Show, Eq, Ord)
data Instruction = Instruction Op Int deriving (Show, Eq, Ord)

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day8" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input8.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 8a" $
                solve8a strs
                `shouldBe`
                1832
            it "passes 8b" $
                solve8b strs
                `shouldBe`
                662

type InstrMap = M.Map Int Instruction

solve8a :: [String] -> Int
solve8a = flip (flip (processInstructions 1 . M.fromList . zip [1..] . map toInstruction) 0) ([])
--solve8a strs = processInstructions 1 (M.fromList $ zip [1..] $ map toInstruction $ strs) 0 []

solve8b :: [String] -> Int
solve8b = tryChanges 1 . M.fromList . zip [1..] . map toInstruction

processInstructions :: Int -> InstrMap -> Int -> [Int] -> Int
processInstructions ip _ acc seen | ip `elem` seen = acc
processInstructions ip instrs acc seen = case M.lookup ip instrs of
    Nothing -> error "bad ip"
    Just (Instruction NOP _) -> processInstructions (ip + 1) instrs acc (ip : seen)
    Just (Instruction ACC num) -> processInstructions (ip + 1) instrs (acc + num) (ip : seen)
    Just (Instruction JMP addr) -> processInstructions (ip + addr) instrs acc (ip : seen)


processInstructionsM :: Int -> InstrMap -> Int -> [Int] -> Maybe Int
processInstructionsM ip _ _ seen | ip `elem` seen = Nothing
processInstructionsM ip instrs acc seen = case M.lookup ip instrs of
    Nothing -> Just acc
    Just (Instruction NOP _) -> processInstructionsM (ip + 1) instrs acc (ip : seen)
    Just (Instruction ACC num) -> processInstructionsM (ip + 1) instrs (acc + num) (ip : seen)
    Just (Instruction JMP addr) -> processInstructionsM (ip + addr) instrs acc (ip : seen)

tryChanges :: Int -> InstrMap -> Int
tryChanges ip instrs = case M.lookup ip instrs of
    Nothing -> error "could not find one"
    Just (Instruction NOP num) -> case processInstructionsM 1 (M.insert ip (Instruction JMP num) instrs) 0 [] of
        Nothing -> tryChanges (ip + 1) instrs
        Just acc -> acc
    Just (Instruction JMP num) -> case processInstructionsM 1 (M.insert ip (Instruction NOP num) instrs) 0 [] of
        Nothing -> tryChanges (ip + 1) instrs
        Just acc -> acc
    _ -> tryChanges (ip + 1) instrs

toInstruction :: String -> Instruction
toInstruction line = Instruction (toOp $ head wrds) (read $ stripped :: Int)
    where
        wrds = words line
        stripped = if (head $ last $ wrds) == '+' then tail $ last wrds else last wrds

toOp :: String -> Op
toOp "nop" = NOP
toOp "jmp" = JMP
toOp "acc" = ACC
toOp _ = error "bad op"
