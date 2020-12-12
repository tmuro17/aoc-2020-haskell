import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Test.Tasty
import Test.Tasty.Hspec

data Direction = North | East | South | West deriving (Show, Eq, Ord)
data Movement = N Int | S Int | W Int | E Int | F Int | L Int | R Int deriving (Show, Eq, Ord)


main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day12" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input12.txt"
    lns <- pure $ T.lines text
    strs <- pure $ map T.unpack lns
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes 12a" $
                solve12a strs
                `shouldBe`
                1533
            it "passes 12b" $
                solve12b strs
                `shouldBe`
                25235


solve12a :: [String] -> Int
solve12a = manhattan (0, 0) . moveShip (0, 0, East) . map toMovement
--solve12a strs = manhattan (0, 0) (moveShip (0,0, East) (map toMovement strs))

toMovement :: String -> Movement
toMovement ('N':xs) = N (read xs)
toMovement ('S':xs) = S (read xs)
toMovement ('W':xs) = W (read xs)
toMovement ('E':xs) = E (read xs)
toMovement ('F':xs) = F (read xs)
toMovement ('L':xs) = L (read xs)
toMovement ('R':xs) = R (read xs)
toMovement _ = error "bad movement"

turn :: Movement -> Direction -> Direction
turn (L num) dir | num == 0 = dir
                 | num < 90 = error "bad degree"
                 | otherwise = turn (L (num - 90)) (left dir)
    where left d | d == North = West
                 | d == West = South
                 | d == South = East
                 | d == East = North
                 | otherwise = error "bad direction"
turn (R num) dir | num == 0 = dir
                 | num < 90 = error "bad degree"
                 | otherwise = turn (R (num - 90)) (right dir)
   where right d | d == North = East
                 | d == East = South
                 | d == South = West
                 | d == West = North
                 | otherwise = error "bad direction"
turn _ _ = error "bad turn"

moveShip :: (Int, Int, Direction) -> [Movement] -> (Int, Int)
moveShip (north, east, _) [] = (north, east)
moveShip (north, east, dir) ((N num) : rst) = moveShip (north + num, east, dir) rst
moveShip (north, east, dir) ((S num) : rst) = moveShip  (north - num, east, dir) rst
moveShip (north, east, dir) ((E num) : rst) = moveShip  (north, east + num, dir) rst
moveShip (north, east, dir) ((W num) : rst) = moveShip  (north, east - num, dir) rst
moveShip (north, east, dir) ((F num) : rst) | dir == North = moveShip (north + num, east, dir) rst
                                            | dir == South = moveShip (north - num, east, dir) rst
                                            | dir == East = moveShip (north, east + num, dir) rst
                                            | dir == West = moveShip (north, east - num, dir) rst
                                            | otherwise = error "bad direction"
moveShip (north, east, dir) ((R deg) : rst) = moveShip (north, east, turn (R deg) dir) rst
moveShip (north, east, dir) ((L deg) : rst) = moveShip (north, east, turn (L deg) dir) rst

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (w, x) (y, z) = abs (w - y) + abs (x - z)
--The monster this could be, like, if you wanted to hate everything
--manhattan = uncurry (flip flip snd . (ap .) . flip flip fst . ((.) .) . (. ((abs .) . (-))) . flip . (((.) . (+) . abs) .) . (-))

solve12b :: [String] -> Int
solve12b = manhattan (0, 0) . moveShipB (0, 0) (1, 10) . map toMovement
--solve12b strs = manhattan (0, 0) (moveShipB (0,0) (1, 10) (map toMovement strs))

moveShipB :: (Int, Int) -> (Int, Int) -> [Movement] -> (Int, Int)
moveShipB (sNorth, sEast) _ [] = (sNorth, sEast)
moveShipB (sNorth, sEast) (wNorth, wEast) ((N num) : rst) = moveShipB (sNorth, sEast) (wNorth + num, wEast) rst
moveShipB (sNorth, sEast) (wNorth, wEast) ((S num) : rst) = moveShipB (sNorth, sEast) (wNorth - num, wEast) rst
moveShipB (sNorth, sEast) (wNorth, wEast) ((E num) : rst) = moveShipB (sNorth, sEast) (wNorth, wEast + num) rst
moveShipB (sNorth, sEast) (wNorth, wEast) ((W num) : rst) = moveShipB (sNorth, sEast) (wNorth, wEast - num) rst
moveShipB (sNorth, sEast) (wNorth, wEast) ((F num) : rst) = moveShipB (sNorth + (wNorth * num), sEast + (wEast * num)) (wNorth, wEast) rst
moveShipB (sNorth, sEast) (wNorth, wEast) ((R deg) : rst) = moveShipB (sNorth, sEast) (rotateR (wNorth, wEast) deg) rst
moveShipB (sNorth, sEast) (wNorth, wEast) ((L deg) : rst) = moveShipB (sNorth, sEast) (rotateL (wNorth, wEast) deg) rst

rotateR :: (Int, Int) -> Int -> (Int, Int)
rotateR (x, y) num | num == 0 = (x, y)
                   | num < 90 = error "bad degree"
                   | otherwise = rotateR (y * (-1), x) (num - 90)

rotateL :: (Int, Int) -> Int -> (Int, Int)
rotateL (x, y) num | num == 0 = (x, y)
                   | num < 90 = error "bad degree"
                   | otherwise = rotateL (y,(-1) * x) (num - 90)


