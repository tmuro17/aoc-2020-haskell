import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
        text <- TIO.readFile "data/input3.txt"
        lns <- pure $ T.lines text
        strs <- pure $ map (T.unpack) lns
        print $ solve3a 0 $ repeatList strs
        print $ product $ map (\s -> solve3b s 0 (repeatList strs)) slopes


solve3a :: Integer -> [String] -> Integer
solve3a n [] = n
solve3a trees lst | hitTree lst = solve3a (trees + 1) transposed
                  | otherwise = solve3a trees transposed
    where
        transposed = overList 3 $ downList 1 lst

slopes :: [(Integer, Integer)]
slopes = [
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)]

solve3b :: (Integer, Integer) -> Integer -> [String] -> Integer
solve3b _ n [] = n
solve3b (r, d) trees lst | hitTree lst = solve3b (r,d) (trees + 1) transposed
                         | otherwise = solve3b (r, d) trees transposed
    where
        transposed = overList r $ downList d lst

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
repeatString = foldr (\s acc -> s ++ acc) "" . repeat
