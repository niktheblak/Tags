module Taglib.Signatures where

import Data.Word
import Taglib.Ascii

sigApe, sigId3v1 :: [Word8]

sigApe = toAscii "APETAGEX"
sigId3v1 = toAscii "TAG"
