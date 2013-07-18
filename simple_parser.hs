import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import Control.Monad

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
    deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

escapeChar :: Char -> Char
escapeChar c =
    case c of
        'a'  -> '\a'
        'b'  -> '\b'
        'f'  -> '\f'
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'
        'v'  -> '\v'
        '\\' -> '\\'
        '\"' -> '\"'
        '\'' -> '\''

escapeableChar :: [Char]
escapeableChar = "abfnrtv\\\"\'"

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ parseNormalCharacter <|> parseEscapeSeq
    char '"'
    return $ String x
        where parseEscapeSeq = char '\\' >> (return . escapeChar =<< oneOf escapeableChar)
              parseNormalCharacter = noneOf "\"\\"

data BaseNumber = BinaryNumber String
                | OctalNumber String
                | DecimalNumber String
                | HexadeciamlNumber String

parseNumber :: Parser LispVal
--parseNumber = (liftM (Number . read)) $ many1 digit
--parseNumber = do
--    x <- many1 digit
--    return $ Number . read $ x
--parseNumber = many1 digit >>= return . Number . read
parseNumber = return . Number . read =<< (normalForm <|> (char '#' >> oneOf "bodx" >> (many1 digit))) -- return the right form in the last expression :)
    where normalForm = many1 digit
          readBase = BinaryNumber c = readBin c --pattern match the types
                   | OctalNumber c = readOct c
                   | DecimalNumber c = read c
                   | HexadecimalNumber c = readHex c


parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ case atom of
            "#t" -> Bool True
            "#f" -> Bool False
            _ -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

main :: IO ()
main = putStrLn.readExpr.(!!0) =<< getArgs

