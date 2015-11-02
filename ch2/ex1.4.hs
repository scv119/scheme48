import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ((char '\\' >> char '"' ) <|> noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> do
  char '#'
  prefix <- letter
  ret <- parseNumberWithPrefix prefix
  return ret

parseDecimal :: Parser LispVal
parseDecimal = liftM (Number . read) $ many1 digit

parseNumberWithPrefix :: Char -> Parser LispVal
parseNumberWithPrefix prefix = liftM (Number . fst . head . prefixRead) $ many1 digit
  where
    prefixRead = case prefix of
      'o' -> readOct
      'd' -> readDec
      'x' -> readHex

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found Value"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
