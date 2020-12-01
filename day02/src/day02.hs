import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Applicative as CA


main :: IO ()
main = do
        text <- TIO.readFile "data/input1.txt"
        let inputTxt = successfulParse text
        print inputTxt

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
