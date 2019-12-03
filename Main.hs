{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import           Control.Applicative
import           Data.Char
import           Numeric
import           System.Exit

data Input = Input
  { inputLoc :: Int
  , inputStr :: String
  } deriving (Show, Eq)

inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ [])       = Nothing
inputUncons (Input loc (x:xs)) = Just (x, Input (loc + 1) xs)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

data ParserError = ParserError Int String deriving (Show)

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (Input, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
        Left $
        ParserError
          (inputLoc input)
          ("Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'")
    f input =
      Left $
      ParserError
        (inputLoc input)
        ("Expected '" ++ [x] ++ "', but reached end of string")

stringP :: String -> Parser String
stringP str =
  Parser $ \input ->
    case runParser (traverse charP str) input of
      Left _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected \"" ++ str ++ "\", but found \"" ++ inputStr input ++ "\"")
      result -> result

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

spanP :: String -> (Char -> Bool) -> Parser String
spanP desc = many . parseIf desc

parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
          Left $
          ParserError
            (inputLoc input)
            ("Expected " ++ desc ++ ", but found '" ++ [y] ++ "'")
      _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected " ++ desc ++ ", but reached end of string")

{-
See page 12 of
http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
-}
doubleLiteral :: Parser Double
doubleLiteral =
  (\sign int frac expo ->
       sign * (int + frac) * (10 ** expo))
    <$> (minus <|> pure 1)
    <*> (read <$> digits)
    <*> opt (read <$> (('0':) <$> ((:) <$> charP '.' <*> digits)))
    <*> opt (e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits)))
  where
    digits = some $ parseIf "digit" isDigit
    minus = (-1) <$ charP '-'
    plus = 1 <$ charP '+'
    e = charP 'e' <|> charP 'E'
    opt = (<|> pure 0)

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
  jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|>
  jsonObject

parseFile :: FilePath -> Parser a -> IO (Either ParserError a)
parseFile fileName parser = do
  input <- readFile fileName
  case runParser parser $ Input 0 input of
    Left e       -> return $ Left e
    Right (_, x) -> return $ Right x

main :: IO ()
main = do
  putStrLn "[INFO] JSON:"
  putStrLn testJsonText
  case runParser jsonValue $ Input 0 testJsonText of
    Right (input, actualJsonAst) -> do
      putStrLn ("[INFO] Parsed as: " ++ show actualJsonAst)
      putStrLn
        ("[INFO] Remaining input (codes): " ++ show (map ord $ inputStr input))
      if actualJsonAst == expectedJsonAst
        then putStrLn "[SUCCESS] Parser produced expected result."
        else do
          putStrLn
            ("[ERROR] Parser produced unexpected result. Expected result was: " ++
             show expectedJsonAst)
          exitFailure
    Left (ParserError loc msg) -> do
      putStrLn $
        "[ERROR] Parser failed at character " ++ show loc ++ ": " ++ msg
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
