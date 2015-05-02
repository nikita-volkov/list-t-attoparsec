module Main where

import BasePrelude
import Test.Hspec
import Data.Text (Text)
import ListT.Attoparsec
import Data.Attoparsec.Text
import qualified ListT.Text
import qualified ListT


main =
  hspec $ do
    context "sample 1" $ do
      let textStream = ListT.Text.stream 2 "abcdefg"
      context "result" $ do
        result <- flip consumeOne textStream $ string "abc" *> string "de" *> char 'f'
        context "head" $ do
          head <- return $ fmap fst result
          it "should be correct" $ shouldBe head (Right 'f')
        context "remainder" $ do
          remainder <- traverse ListT.toList $ fmap snd result
          it "should be correct" $ shouldBe remainder (Right ["g"])
