module Taglib.Exceptions where

import Data.Typeable(Typeable)
import Control.Exception(Exception)

data TaglibException = TaglibIOException String | TaglibFormatException String
    deriving (Show, Typeable)

instance Exception TaglibException