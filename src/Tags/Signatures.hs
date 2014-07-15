module Tags.Signatures where

import Data.Word
import Tags.Ascii

sigApe, sigId3v1 :: [Word8]

sigApe = toAscii "APETAGEX"
sigId3v1 = toAscii "TAG"
