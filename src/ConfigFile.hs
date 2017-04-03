{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigFile
    ( getVal
    , readConfigFile
    , Config
    ) where

import ClassyPrelude hiding (many, try, optional)

import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import Data.Dynamic

newtype Config = Config (Map Text Text)
    deriving (Show, Read)

type Setting = (Text, Text)

identifier :: Parser Text
identifier = do
  c <- letter
  rest <- many alphaNum
  spaces
  return $ pack $ c : rest

parseBrackets :: Parser Text
parseBrackets = brackets (pack <$> manyTill anyChar (lookAhead $ char ']')) <* spaces

parseValue :: Parser Text
parseValue = (pack <$> manyTill anyChar eol) <* spaces

parseKey :: Parser (Text, Text)
parseKey = do
  k <- identifier
  _ <- char '=' <* spaces
  v <- parseBrackets <|> parseValue
  return (k, v)

brackets :: Parser Text -> Parser Text
brackets p = do
  res <- between (char '[') (char ']') p
  return $ "[" <> res <> "]"

comment :: Parser Text
comment = spaces *> char '#' *> (pack <$> manyTill anyChar eol) <* spaces

eol :: Parser ()
eol = (optional (char '\r') >> newline >> return ()) <|> eof

parseCfg :: Parser Config
parseCfg = Config <$> mapFromList <$> many parseKey

readConfigFile :: (MonadThrow m, MonadIO m) => FilePath -> m Config
readConfigFile path = do
    cts <- readFileUtf8 path
    case parse parseCfg "Read Config" cts of
      Left parseError -> throwM $ ConfigError $ tshow parseError
      Right config -> return config

-- parseCfg :: Parser Config
-- parseCfg = Config <$> mapFromList <$> lines
--     where
--       lines = many parseLine

-- parseLine :: Parser Setting
-- parseLine = (,) <$> key <*> value
--     where
--       key = parseKey <?> "parseKey"
--       value = intercalate " " <$> parseTokens

-- parseTokens :: Parser [Text]
-- parseTokens = manyTill parseToken (try (lookAhead parseKey >> return ()) <|> try (spaces >> eof))

-- parseKey :: Parser Text
-- parseKey = pack <$> tok
--     where
--       tok = spaces *> manyTill anyChar (lookAhead space <|> lookAhead (char '=')) <* (spaces >> char '=')

-- parseToken :: Parser Text
-- parseToken = pack <$> (spaces *> tok) <?> "Token"
--     where
--       -- The lookAhead is to ensure that this parser fails on the empty string
--       tok = lookAhead anyChar >> manyTill anyChar (lookAhead (space >> return ()) <|> eof)

-- -- parseBracket :: Char -> Char -> Parser Text
-- -- parseBracket = runScanner 0 scanBracket

-- scanBracket :: Char -> Char -> Int -> Char -> Maybe Int
-- scanBracket _ _ 0 _ = Nothing
-- scanBracket open close n c
--   | c == open = Just (n + 1)
--   | c == close = Just (n - 1)
--   | otherwise = Just n

data ConfigError = ConfigError Text
    deriving Typeable

instance Show ConfigError where
    show (ConfigError msg) = "ConfigError: " <> show msg

instance Exception ConfigError

getVal :: (MonadThrow m, Read a, Typeable a) => Text -> Config -> m a
getVal key (Config cfg) = do
  case lookup key cfg of
    Nothing -> throwM $ ConfigError $ "Could not find key " <> key <> " in configuration file"
    Just txtVal -> do
        let mVal = readMay txtVal
        case mVal of
          Nothing -> throwM $ ConfigError $ "key: " <> key <> " does not appear to be of type: " <> dispType mVal
          Just val -> return val

dispType :: Typeable a => Maybe a -> Text
dispType = pack . intercalate " " . fmap show . typeRepArgs . typeOf

-- readConfigFile :: (MonadThrow m, MonadIO m) => FilePath -> m Config
-- readConfigFile path = do
--     cts <- readFile path
--     case parse parseCfg "Read Config" cts of
--       Left parseError -> throwM $ ConfigError $ tshow parseError
--       Right config -> return config
