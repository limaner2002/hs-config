{-# LANGUAGE OverloadedStrings #-}

module ConfigFile
    ( getVal
    , readConfigFile
    , Config
    ) where

import ClassyPrelude hiding (many, try)
import Prelude ()

import Text.Parsec hiding ((<|>))
import Text.Parsec.Text
import Data.Dynamic

newtype Config = Config (Map Text Text)
    deriving (Show, Read)

type Setting = (Text, Text)

parseCfg :: Parser Config
parseCfg = Config <$> mapFromList <$> lines
    where
      lines = many parseLine

parseLine :: Parser Setting
parseLine = (,) <$> key <*> value
    where
      key = parseKey <?> "parseKey"
      value = intercalate " " <$> parseTokens

parseTokens :: Parser [Text]
parseTokens = manyTill parseToken (try (lookAhead parseKey >> return ()) <|> try (spaces >> eof))

parseKey :: Parser Text
parseKey = pack <$> tok
    where
      tok = spaces *> manyTill anyChar (lookAhead space <|> lookAhead (char '=')) <* (spaces >> char '=')

parseToken :: Parser Text
parseToken = pack <$> (spaces *> tok) <?> "Token"
    where
      -- The lookAhead is to ensure that this parser fails on the empty string
      tok = lookAhead anyChar >> manyTill anyChar (lookAhead (space >> return ()) <|> eof)

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

readConfigFile :: (MonadThrow m, MonadIO m) => FilePath -> m Config
readConfigFile path = do
    cts <- readFile path
    case parse parseCfg "Read Config" cts of
      Left parseError -> throwM $ ConfigError $ tshow parseError
      Right config -> return config