import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.Hspec
import Safe
import Data.Maybe

data State = Filled | Empty | Floor deriving (Show, Eq, Ord)
type SeatMap = [[State]]

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day11" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input11.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 11a" $
                solve11a strs
                `shouldBe`
                2126
            it "passes 11b" $
                solve11b strs
                `shouldBe`
                1914

solve11a :: [String] -> Int
solve11a strs = length $ filter (== Filled) $ concat $ stepUntilStable $ toSeatingMap strs


solve11b :: [String] -> Int
solve11b strs = length $ filter (== Filled) $ concat $ stepUntilStableB $ toSeatingMap strs

toSeatingMap :: [String] -> SeatMap
toSeatingMap = map (map toState)

toState :: Char -> State
toState x | x == 'L' = Empty
          | x == '.' = Floor
          | x == '#' = Filled
          | otherwise = error "invalid state"

stepMap :: SeatMap -> (Int, Int) -> SeatMap -> SeatMap
stepMap orig (row, col) new | row >= maxRow = init new
                            | col >= maxCol = stepMap orig (succ row, 0) (new ++ [[]])
                            | currentState == Just Empty && occupiedSeats == 0 = stepMap orig (row, succ col) (init new ++ [last new ++ [Filled]])
                            | currentState == Just Filled && occupiedSeats >= 4 = stepMap orig (row, succ col) (init new ++ [last new ++ [Empty]])
                            | otherwise = stepMap orig (row, succ col) (init new ++ [last new ++ [fromJust currentState]])
    where
        maxRow = length orig
        maxCol = length $ head orig
        neighborLocations :: [(Int, Int)]
        neighborLocations = [(pred row, pred col), (pred row, col), (pred row, succ col),
                             (row, pred col), (row, succ col),
                              (succ row, pred col), (succ row, col), (succ row, succ col)]
        lookLoc :: SeatMap -> (Int, Int) -> Maybe State
        lookLoc seatMap (r, c) = (seatMap `atMay` r) >>= (`atMay` c)
        neighborStates :: [State]
        neighborStates = catMaybes $ map (lookLoc orig) neighborLocations
        currentState = lookLoc orig (row, col)
        occupiedSeats = length $ filter (== Filled) neighborStates

stepUntilStable :: SeatMap -> SeatMap
stepUntilStable input | input == next = next
                      | otherwise = stepUntilStable next
    where next = stepMap input (0,0) [[]]

stepMapB :: SeatMap -> (Int, Int) -> SeatMap -> SeatMap
stepMapB orig (row, col) new | row >= maxRow = init new
                               | col >= maxCol = stepMapB orig (succ row, 0) (new ++ [[]])
                               | currentState == Just Empty && occupiedSeats == 0 = stepMapB orig (row, succ col) (init new ++ [last new ++ [Filled]])
                               | currentState == Just Filled && occupiedSeats >= 5 = stepMapB orig (row, succ col) (init new ++ [last new ++ [Empty]])
                               | otherwise = stepMapB orig (row, succ col) (init new ++ [last new ++ [fromJust currentState]])
    where
        maxRow = length orig
        maxCol = length $ head orig
        neighborDirections :: [(Int, Int)]
        neighborDirections = [((-1), (-1)), ((-1), (0)), ((-1), 1),
                              (0, (-1)), (0, 1),
                              (1, (-1)), (1, 0), (1, 1)]
        lookDirection :: SeatMap -> (Int, Int) -> (Int,Int) -> Maybe State
        lookDirection seatMap (r, c) (down, right) = case (seatMap `atMay` (r + down)) >>= (`atMay` (c + right)) of
            Nothing -> Nothing
            Just Floor -> lookDirection seatMap (r + down, c + right) (down, right)
            x -> x
        lookLoc :: SeatMap -> (Int, Int) -> Maybe State
        lookLoc seatMap (r, c) = (seatMap `atMay` r) >>= (`atMay` c)
        neighborStates :: [State]
        neighborStates = catMaybes $ map (lookDirection orig (row, col)) neighborDirections
        currentState = lookLoc orig (row, col)
        occupiedSeats = length $ filter (== Filled) neighborStates

stepUntilStableB :: SeatMap -> SeatMap
stepUntilStableB input | input == next = next
                         | otherwise = stepUntilStableB next
    where next = stepMapB input (0,0) [[]]
