{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Applicative
import           Data.Char
import           Data.Tuple
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

newtype Parser a = Parser
  { runParser :: String -> Either String (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Right (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Right (input'', f a)

instance Alternative Parser where
  empty = Parser $ const (Left "nothing to parse")
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
      Right s -> Right s
      Left  _ -> p2 input

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Right (ys, x)
      | otherwise = Left $ "expected " ++ show x ++ " got " ++ show y
    f [] = Left "nothing to parse"

failP :: String -> Parser a
failP s = Parser (\s' -> Left ("expected " ++ show s ++ " got " ++ show s'))

stringP :: String -> Parser String
stringP s = traverse charP s <|> failP s

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

spanP :: (Char -> Bool) -> Parser String
spanP = many . parseIf

spanP1 :: (Char -> Bool) -> Parser String
spanP1 = some . parseIf

parseIf :: (Char -> Bool) -> Parser Char
parseIf f =
  Parser $ \case
    y:ys
      | f y -> Right (ys, y)
    _ -> Left "predicate failed"

listToEither :: [a] -> Either String a
listToEither (x:_) = Right x
listToEither [] = Left "got empty list"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber <$> Parser (fmap swap . listToEither . reads)

escapeUnicode :: Parser Char
escapeUnicode = chr . fst . head . readHex <$> sequenceA (replicate 4 (parseIf isHexDigit))

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
normalChar = parseIf ((&&) <$> (/= '"') <*> (/= '\\'))

stringLiteral :: Parser String
stringLiteral = charP '"' *> many (normalChar <|> escapeChar) <* charP '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanP isSpace

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
  jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|>
  jsonObject

parseFile :: FilePath -> Parser a -> IO (Either String a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = do
  putStrLn "[INFO] JSON:"
  putStrLn testJsonText
  case runParser jsonValue testJsonText of
    Right (input, actualJsonAst) -> do
      putStrLn ("[INFO] Parsed as: " ++ show actualJsonAst)
      putStrLn ("[INFO] Remaining input (codes): " ++ show (map ord input))
      if actualJsonAst == expectedJsonAst
        then putStrLn "[SUCCESS] Parser produced expected result."
        else do
          putStrLn
            ("[ERROR] Parser produced unexpected result. Expected result was: " ++
             show expectedJsonAst)
          exitFailure
    Left s -> do
      putStrLn "[ERROR] Parser failed with error:"
      putStrLn s
      exitFailure
  where
    testJsonText =
      unlines
        [ "{"
        , "    \"hello\": [false, true, null, 42, \"foo\\n\\u1234\\\"\", [1, -2, 3.1415, 4e-6, 5E6]],"
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
                  ]
              ])
        , ("world", JsonNull)
        ]
