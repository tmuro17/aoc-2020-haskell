import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA

import Test.Tasty
import Test.Tasty.Hspec

import Control.Monad (guard)

main :: IO ()
main = do
    spc <- spec
    tSpec <- testSpec "day1" spc
    defaultMain (testGroup "Tests" [tSpec])

spec :: IO (SpecWith ())
spec = do
    text <- TIO.readFile "data/input1.txt"
    let inputTxt = successfulParse text
    return $ context "spec" $ do
        describe "overall" $ do
            it "passes example" $
                (head $ solve1a [1721,979,366,299,675,1456]) `shouldBe` 514579
            it "passes 1a" $
                (head $ solve1a inputTxt) `shouldBe` 471019
            it "passes 1b" $
                (head $ solve1b inputTxt) `shouldBe` 103927824
-- Parse the input file
type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space (skipSome spaceChar) CA.empty CA.empty
-- sc = L.space (skipSome (char ' ')) CA.empty CA.empty

lexeme  = L.lexeme sc
integer = lexeme L.decimal
-- signedInteger = L.signed sc integer

moduleP = many integer

successfulParse :: Text -> [Integer]
successfulParse input =
        case parse moduleP "input" input of
                Left  _err -> [] -- TIO.putStr $ T.pack $ parseErrorPretty err
                Right nums -> nums

solve1a :: [Integer] -> [Integer]
solve1a ints = do
    x <- ints
    y <- ints
    guard (x + y == 2020)
    return $ (x * y)

solve1b :: [Integer] -> [Integer]
solve1b ints = do
    x <- ints
    y <- ints
    z <- ints
    guard (x + y + z == 2020)
    return $ (x * y * z)
