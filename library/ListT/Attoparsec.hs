module ListT.Attoparsec where

import BasePrelude hiding (cons, uncons)
import MTLPrelude
import Data.Text (Text)
import ListT
import qualified Data.Attoparsec.Text as P


data ParsingFailure =
  ParsingFailure !String ![String]
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

textParser :: MonadError ParsingFailure m => P.Parser a -> Transformation m Text a
textParser p =
  loop (P.parse p)
  where
    loop parse input =
      lift (uncons input) >>= \case
        Nothing -> mzero
        Just (chunk, otherChunks) -> 
          case parse chunk of
            P.Done chunk' result -> 
              cons result (textParser p (cons chunk' otherChunks))
            P.Partial parse' -> 
              loop parse' otherChunks
            P.Fail _ contexts message -> 
              lift $ throwError $ ParsingFailure message contexts


