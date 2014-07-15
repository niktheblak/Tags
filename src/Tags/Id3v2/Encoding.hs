module Tags.Id3v2.Encoding(Encoding(..),
                           enctoid,
                           idtoenc,
                           defaultEncoding,
                           encode) where

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Char
import Data.Word

-- | Possible text encodings for ID3v2 text frame types.
data Encoding =
    -- | ISO 8859-1 (Latin1).
    ISO8859 |
    -- | UTF-16 with byte order mark.
    UTF16 |
    -- | UTF-16 big endian.
    UTF16BE |
    -- | UTF-8.
    UTF8 deriving (Eq, Ord, Show)

enctoid :: Encoding -> Word8
enctoid ISO8859 = 0
enctoid UTF16 = 1
enctoid UTF16BE = 2
enctoid UTF8 = 3

idtoenc :: Word8 -> Encoding
idtoenc 0 = ISO8859
idtoenc 1 = UTF16
idtoenc 2 = UTF16BE
idtoenc 3 = UTF8
idtoenc _ = error "Unknown encoding."

-- | The default text encoding used by this library.
defaultEncoding :: Encoding
defaultEncoding = UTF8

encode :: Encoding -> String -> [Word8]
encode enc str =
    case enc of
        ISO8859 -> map (fromIntegral . ord) str
        UTF8 -> UTF8.encode str
        otherwise -> error "Unknown encoding."
