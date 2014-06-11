-- | Functions and types for reading and writing textual key-value pairs as
-- Vorbis items.
module Taglib.Vorbis.VorbisItem(VorbisItem,
                                 createVorbisItem,
                                 itemSize,
                                 isValidKey,
                                 toVorbisItem,
                                 writeItem,
                                 readItem) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString.Char8 as BSC
import Control.Exception(throw)
import Data.Char
import System.IO
import System.Log.Logger

import Taglib.Ascii
import Taglib.BinaryIO
import Taglib.Exceptions
import Taglib.ItemData

data VorbisItem = VorbisItem String String deriving (Eq, Ord)

instance Show VorbisItem where
    show (VorbisItem k v) = concat [k, " = ", v]

instance TagItem VorbisItem where
    key (VorbisItem k _) = k
    value (VorbisItem _ v) = Text v

instance HasItemValue VorbisItem where
    itemValue (VorbisItem _ v) = Text v

separator :: Char
separator = '='

createVorbisItem :: String -> String -> VorbisItem
createVorbisItem key value = if isValidKey key
    then VorbisItem key value
    else throw (TaglibInvalidKeyException "Invalid Vorbis item key")

-- | Gets the size that a vorbis item will take when serialized.
itemSize :: VorbisItem -> Int
itemSize (VorbisItem key value) = length key + 1 + length (UTF8.encode value)

-- | Determines whether the given string is a valid Vorbis item key.
isValidKey :: String -> Bool
isValidKey key =
    (not . null) key && all validChar key
    where
        validChar c
            | ord c < 0x20 = False
            | c == separator = False
            | ord c > 0x7D = False
            | otherwise = True

-- | Converts a 'TagItem' instance into a Vorbis item if the key is a valid
-- Vorbis item key and the value is of type 'Text'.
toVorbisItem :: (TagItem a) => a -> Maybe VorbisItem
toVorbisItem item =
    let k = key item in
    if isValidKey k
        then case value item of
            Text t -> Just (VorbisItem k t)
            _ -> Nothing
        else Nothing

-- | Writes a Vorbis item to a specified handle.
writeItem :: Handle -> VorbisItem -> IO ()
writeItem handle (VorbisItem key value) =
    let asciiKey = toAscii key
        utfData = UTF8.encode value
        itemLen = length asciiKey + 1 + length utfData
    in do
        write32LE handle (toEnum itemLen)
        writeData handle asciiKey
        hPutChar handle separator
        writeData handle utfData

-- | Reads a Vorbis item from the specified handle.
readItem :: Handle -> IO VorbisItem
readItem handle = do
        -- Read the item size.
        itemSize <- read32LE handle
        debugM "VorbisItem.readItem" ("Item size: " ++ show itemSize)
        if itemSize > 65536 then
            warningM "VorbisItem.readItem"
                ("Item size (" ++ show itemSize ++ ") is probably invalid.")
            else return ()
        -- Read the item data.
        itemData <- BSC.hGet handle (fromEnum itemSize)
        let (key, value) = BSC.break (\c -> c == separator) itemData
            k = BSC.unpack key
            v = BSC.unpack (BSC.tail value) in
            return (VorbisItem k v)