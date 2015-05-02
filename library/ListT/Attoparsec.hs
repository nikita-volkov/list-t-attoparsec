module ListT.Attoparsec where

import BasePrelude hiding (cons, uncons)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Data.Either.Combinators
import Data.Text (Text)
import ListT
import qualified Data.Attoparsec.Text as P


-- |
-- A text message and a list of contexts,
-- as per the failure in \"attoparsec\".
data Error =
  Error !String ![String]
  deriving (Show, Eq, Ord, Data, Typeable, Generic)


-- |
-- Given a text parser, produces 
-- a transformation of a stream of text chunks into a stream of parsed results.
-- In case of a parsing failure it raises an 'Error' in the 'EitherT' monad over the base.
textParser :: Monad m => P.Parser a -> ListT m Text -> ListT (EitherT Error m) a
textParser p =
  loop (P.parse p)
  where
    loop parse input =
      lift (lift (uncons input)) >>= maybe mzero (onUncons parse)
    onUncons parse (chunk, otherChunks) =
      case parse chunk of
        P.Done chunk' result -> 
          cons result (onUncons (P.parse p) (chunk', otherChunks))
        P.Partial parse' -> 
          loop parse' otherChunks
        P.Fail _ contexts message -> 
          lift $ EitherT $ return $ Left $ Error message contexts


