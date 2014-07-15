module Tags.Exceptions where

import Data.Typeable(Typeable)
import Control.Exception(Exception)

data TagsException = TagsIOException String
        | TagsFormatException String
        | TagsInvalidKeyException String
    deriving (Show, Typeable)

instance Exception TagsException