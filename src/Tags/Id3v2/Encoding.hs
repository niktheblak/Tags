module Tags.Id3v2.Encoding(Encoding(..),
                           enctoid,
                           idtoenc,
                           defaultEncoding,
                           encode) where

import qualified Data.ByteString as BS
import Data.Char
import qualified Data.Encoding as Enc
import qualified Data.Encoding.ISO88591 as EncISO88591
import qualified Data.Encoding.UTF8 as EncUTF8
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

encode :: Encoding -> String -> BS.ByteString
encode enc str =
    case enc of
        ISO8859 -> Enc.encodeStrictByteString EncISO88591.ISO88591 str
        UTF8 -> Enc.encodeStrictByteString EncUTF8.UTF8 str
        otherwise -> error "Unknown encoding."
