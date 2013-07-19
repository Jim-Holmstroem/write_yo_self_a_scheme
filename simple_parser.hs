import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
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

readBin = readBin_reverse . reverse
    where readBin_reverse ('0':xs) = 0 + 2 * readBin_reverse xs
          readBin_reverse ('1':xs) = 1 + 2 * readBin_reverse xs
          readBin_reverse [] = 0

(readOct, readDec, ReadHex) = map (fst . (!!0) $) [Numeric.readOct, Numeric.ReadDec, Numeric.ReadHex]

parseNumber :: Parser LispVal
--parseNumber = (liftM (Number . read)) $ many1 digit
--parseNumber = do
--    x <- many1 digit
--    return $ Number . read $ x
--parseNumber = many1 digit >>= return . Number . read
parseNumber = return . Number =<<
    (
        return . readDec =<< many1 digit <|>
        (
            char '#' >>
                (return . readBin =<< (char "b" >> (many1 binaryDigit))) <|>
                (return . readOct =<< (char "o" >> (many1 octalDigit))) <|>
                (return . readDec =<< (char "d" >> (many1 decimalDigit))) <|>
                (return . readHex =<< (char "x" >> (many1 hexadecimalDigit)))
        )
    )
    where binaryDigit = "01"
          octalDigit = "01234567"
          decimalDigit = "01234567" --TODO make this a generalFunction tokensUpTo "a", remember to use lowercase
          hexadecimalDigit = "0123456789abcdef"

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

