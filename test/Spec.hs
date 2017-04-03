{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ClassyPrelude
import Prelude ()

import ConfigFile
import Test.Hspec

data TestSetting1 = TestSetting1
    { f1 :: Int
    , f2 :: String
    , f3 :: ByteString
    } deriving (Show, Eq)

data TestSetting2 = TestSetting2
    { f4 :: [Int]
    , f5 :: (String, Float)
    } deriving (Show, Eq)

readTestSetting1 :: MonadThrow m => Config -> m TestSetting1
readTestSetting1 cfg =
    TestSetting1 <$> getVal' "f1"
                 <*> getVal' "f2"
                 <*> getVal' "f3"
    where
      getVal' k = getVal k cfg

readTestSetting2 :: MonadThrow m => Config -> m TestSetting2
readTestSetting2 cfg =
    TestSetting2 <$> getVal' "f4"
                 <*> getVal' "f5"
    where
      getVal' k = getVal k cfg

main :: IO ()
main = hspec $ do
  describe "test1" $ do
     it "can parse a simple config file to a simple type" $ do
          cfg <- readConfigFile "test/settings1.conf"
          setting1 <- readTestSetting1 cfg
          setting1 `shouldBe` TestSetting1 {f1 = 73, f2 = "strVal", f3 = "ByteString value"}

     it "can parse a simple config file to higher kinded types" $ do
          cfg <- readConfigFile "test/settings2.conf"
          setting2 <- readTestSetting2 cfg
          setting2 `shouldBe` TestSetting2 {f4 = [1,73,-4], f5 = ("delay", 43.5)}

     it "can parse a simple config file that has trailing empty space" $ do
          cfg <- readConfigFile "test/settings3.conf"
          setting1 <- readTestSetting1 cfg
          setting1 `shouldBe` TestSetting1 {f1 = 73, f2 = "strVal", f3 = "ByteString value"}

     it "can handle carriage returns in newlines" $ do
          cfg <- readConfigFile "test/settings4.conf"
          setting1 <- readTestSetting1 cfg
          setting1 `shouldBe` TestSetting1 {f1 = 73, f2 = "strVal", f3 = "ByteString value"}
