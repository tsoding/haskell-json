{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Applicative
import           Data.Char
import           Numeric
import           System.Exit

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

data ParserError = UnknownError | KnownError (Int, String) deriving (Show)

newtype Parser a = Parser
  { runParserWithLoc :: Int -> String -> Either ParserError (Int, String, a)
  }

runParser :: Parser a -> String -> Either ParserError (Int, String, a)
runParser parser = runParserWithLoc parser 0

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \loc input -> do
      (loc', input', x) <- p loc input
      return (loc', input', f x)

instance Applicative Parser where
  pure x = Parser $ \loc input -> Right (loc, input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \loc input -> do
      (loc', input', f) <- p1 loc input
      (loc'', input'', a) <- p2 loc' input'
      return (loc'', input'', f a)

instance Alternative (Either ParserError) where
  empty = Left UnknownError
  Left UnknownError <|> e2       = e2
  e1@(Left (KnownError (loc1, _))) <|> e2@(Left (KnownError (loc2, _)))
      | loc2 > loc1 = e2
      | otherwise   = e1
  Left _             <|> Right x = Right x
  e1                 <|> _       = e1

instance Alternative Parser where
  empty = Parser $ \_ _ -> empty
  (Parser p1) <|> (Parser p2) = Parser $ \loc input ->
                                           p1 loc input <|> p2 loc input

(<?>) :: Parser a -> String -> Parser a
(Parser p) <?> defaultMsg = Parser $ \loc input ->
  let result = p loc input
      defaultError = Left $ KnownError (loc, defaultMsg) in
  case result of
    Left UnknownError -> defaultError
    Left (KnownError (loc', _))
      | loc == loc' -> defaultError
      | otherwise   -> result
    Right _ -> result

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f loc (y:ys)
      | y == x = Right (loc+1, ys, x)
      | otherwise = Left $ KnownError (loc, "Expected '" ++ [x] ++ "', but found " ++ [y])
    f loc [] = Left $ KnownError (loc, "Expected '" ++ [x] ++ "', but reached end of string")

stringP :: String -> Parser String
stringP str = Parser $ \loc input ->
  let result = runParserWithLoc (traverse charP str) loc input in
  case result of
    Left (KnownError _) -> Left $ KnownError (loc, "Expected \"" ++ str ++ "\", but found \"" ++ input ++ "\"")
    _                   -> result

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

spanP :: String -> (Char -> Bool) -> Parser String
spanP desc = many . parseIf desc

spanP1 :: String -> (Char -> Bool) -> Parser String
spanP1 desc = some . parseIf desc

parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf desc f =
  Parser $ \loc input ->
    case input of
      y:ys
        | f y       -> Right (loc+1, ys, y)
        | otherwise -> Left $ KnownError (loc, "Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      [] -> Left $ KnownError (loc, "Expected " ++ desc ++ ", but reached end of string")

{-
A diagram explaining the logic behind the functions
decimalLiteral, nonNegativeLiteral, and doubleLiteral
can be found on page 12 of
http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
-}
decimalLiteral :: Parser (Double, Integer)
decimalLiteral = (readDecimalPart <$>
                  ((charP '.' *> spanP1 "digit" isDigit) <|>
                   pure "0")
                  <*>
                  (((charP 'e' <|> charP 'E') *>
                    (readExponent <$>
                     ((charP '+' <|> charP '-') <|> pure '+') <*>
                     spanP1 "digit" isDigit))
                   <|>
                   pure 0)) <|>
                  pure (0, 0)
  where readExponent '+' = read
        readExponent '-' = negate . read
        readExponent ch  = error $ ch : " was passed into readExponent, which expects either '+' or '-'"
        
        readDecimalPart digits expnt = (read digits * 10**(-offset), expnt)
          where offset = fromIntegral (length digits)

nonNegativeLiteral :: Parser Double
nonNegativeLiteral = (charP '0' *>
                      fmap (readNonNegativePart '0' []) decimalLiteral) <|>
                     (readNonNegativePart <$>
                      parseIf "positive digit" isPositiveDigit <*>
                      spanP "digit" isDigit <*>
                      decimalLiteral)
  where isPositiveDigit = (&&) <$> isDigit <*> (/= '0')
        readNonNegativePart firstDigit restDigits (decimal, expnt)
          = (read (firstDigit:restDigits) + decimal)*(10**exponentDouble)
          where exponentDouble = fromIntegral expnt

doubleLiteral :: Parser Double
doubleLiteral = (charP '-' *> fmap negate nonNegativeLiteral) <|> nonNegativeLiteral

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> doubleLiteral

escapeUnicode :: Parser Char
escapeUnicode = chr . fst . head . readHex <$> sequenceA (replicate 4 (parseIf "hex digit" isHexDigit))

escapeChar :: Parser Char
escapeChar = ('"' <$ stringP "\\\"") <|>
             ('\\' <$ stringP "\\\\") <|>
             ('/' <$ stringP "\\/") <|>
             ('\b' <$ stringP "\\b") <|>
             ('\f' <$ stringP "\\f") <|>
             ('\n' <$ stringP "\\n") <|>
             ('\r' <$ stringP "\\r") <|>
             ('\t' <$ stringP "\\t") <|>
             (stringP "\\u" *> escapeUnicode)

normalChar :: Parser Char
normalChar = parseIf "non-special character" ((&&) <$> (/= '"') <*> (/= '\\'))

stringLiteral :: Parser String
stringLiteral = charP '"' *> many (normalChar <|> escapeChar) <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP "whitespace character" isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonObject :: Parser JsonValue
jsonObject =
  JsonObject <$>
  (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
  where
    pair = liftA2 (,) (stringLiteral <* ws <* charP ':' <* ws) jsonValue

jsonValue :: Parser JsonValue
jsonValue =
  (jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|>
   jsonObject)
  <?>
  "Could not parse the given expression"

parseFile :: FilePath -> Parser a -> IO (Either ParserError a)
parseFile fileName parser = do
  input <- readFile fileName
  case runParser parser input of
      Left e          -> return $ Left e
      Right (_, _, x) -> return $ Right x

main :: IO ()
main = do
  putStrLn "[INFO] JSON:"
  putStrLn testJsonText
  case runParser jsonValue testJsonText of
    Right (_, input, actualJsonAst) -> do
      putStrLn ("[INFO] Parsed as: " ++ show actualJsonAst)
      putStrLn ("[INFO] Remaining input (codes): " ++ show (map ord input))
      if actualJsonAst == expectedJsonAst
        then putStrLn "[SUCCESS] Parser produced expected result."
        else do
          putStrLn
            ("[ERROR] Parser produced unexpected result. Expected result was: " ++
             show expectedJsonAst)
          exitFailure
    Left (KnownError (loc, msg)) -> do
      putStrLn $ "[ERROR] Parser failed at character " ++
                 show loc ++
                 ": " ++
                 msg
      exitFailure
    Left UnknownError -> do
      putStrLn "[ERROR] Parser failed and didn't produce any output."
      exitFailure
  where
    testJsonText =
      unlines
        [ "{"
        , "    \"hello\": [false, true, null, 42, \"foo\\n\\u1234\\\"\", [1, -2, 3.1415, 4e-6, 5E6, 0.123e+1]],"
        , "    \"world\": null"
        , "}"
        ]
    expectedJsonAst =
      JsonObject
        [ ( "hello"
          , JsonArray
              [ JsonBool False
              , JsonBool True
              , JsonNull
              , JsonNumber 42
              , JsonString "foo\n\4660\""
              , JsonArray
                  [ JsonNumber 1.0
                  , JsonNumber (-2.0)
                  , JsonNumber 3.1415
                  , JsonNumber 4e-6
                  , JsonNumber 5000000
                  , JsonNumber 1.23
                  ]
              ])
        , ("world", JsonNull)
        ]
