{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import           Data.Char
import           Data.Semigroup
import           Numeric
import           System.Exit

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer -- NOTE: no support for floats
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- NOTE: no proper error reporting
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

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
      | f y -> Just (ys, y)
    _ -> Nothing

jsonNumber :: Parser JsonValue
jsonNumber = f <$> spanP1 isDigit
  where
    f ds = JsonNumber $ read ds

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

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = do
  putStrLn "[INFO] JSON:"
  putStrLn testJsonText
  case runParser jsonValue testJsonText of
    Just (input, actualJsonAst) -> do
      putStrLn ("[INFO] Parsed as: " <> show actualJsonAst)
      putStrLn ("[INFO] Remaining input (codes): " <> show (map ord input))
      if actualJsonAst == expectedJsonAst
        then putStrLn "[SUCCESS] Parser produced expected result."
        else do
          putStrLn
            ("[ERROR] Parser produced unexpected result. Expected result was: " <>
             show expectedJsonAst)
          exitFailure
    Nothing -> do
      putStrLn "[ERROR] Parser failed and didn't produce any output."
      exitFailure
  where
    testJsonText =
      unlines
        [ "{"
        , "    \"hello\": [false, true, null, 42, \"foo\\n\\u1234\\\"\", [1, 2, 3, 4]],"
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
                  [JsonNumber 1, JsonNumber 2, JsonNumber 3, JsonNumber 4]
              ])
        , ("world", JsonNull)
        ]
