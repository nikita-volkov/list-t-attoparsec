module ListT.Attoparsec where

import BasePrelude hiding (cons, uncons)
import MTLPrelude
import Data.Text (Text)
import ListT
import qualified Data.Attoparsec.Text as P


-- |
-- A text message and a list of contexts,
-- as per the failure in \"attoparsec\".
data ParsingFailure =
  ParsingFailure !String ![String]
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

-- |
-- Given a text parser, produces 
-- a transformation of a stream of text chunks into a stream of parsed results.
-- In case of a parsing failure it raises a 'ParsingFailure' error in the base monad.
textParser :: MonadError ParsingFailure m => P.Parser a -> Transformation m Text a
textParser p =
  loop (P.parse p)
  where
    loop parse input =
      lift (uncons input) >>= maybe mzero (onUncons parse)
    onUncons parse (chunk, otherChunks) =
      case parse chunk of
        P.Done chunk' result -> 
          cons result (onUncons (P.parse p) (chunk', otherChunks))
        P.Partial parse' -> 
          loop parse' otherChunks
        P.Fail _ contexts message -> 
          lift $ throwError $ ParsingFailure message contexts


