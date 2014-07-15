-- | Functions and types for reading and writing textual key-value pairs as
-- Vorbis items.
module Tags.Vorbis.VorbisItem(VorbisItem,
                              createVorbisItem,
                              itemSize,
                              isValidKey,
                              toVorbisItem,
                              getVorbisItem,
                              putVorbisItem,
                              writeItem,
                              readItem) where

import Control.Exception(throw)
import Control.Monad(when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Binary.Get
import Data.Binary.Put
import Data.Char
import qualified Data.Encoding as Enc
import Data.Encoding.ASCII
import Data.Encoding.UTF8
import System.IO
import System.Log.Logger

import Tags.BinaryIO
import Tags.Exceptions
import Tags.ItemData

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
    else throw (TagsInvalidKeyException "Invalid Vorbis item key")

-- | Gets the size that a vorbis item will take when serialized.
itemSize :: VorbisItem -> Int
itemSize (VorbisItem key value) = length key + 1 + length (Enc.encodeString UTF8 value)

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

putVorbisItem :: VorbisItem -> Put
putVorbisItem (VorbisItem key value) =
    let keyData = Enc.encodeStrictByteString ASCII key
        valueData = Enc.encodeStrictByteString UTF8 value
        itemSize = BS.length keyData + 1 + BS.length valueData
    in do
        putWord32le (toEnum itemSize)
        putByteString keyData
        putWord8 (toEnum (ord separator))
        putByteString valueData

getVorbisItem :: Get VorbisItem
getVorbisItem = do
    itemSize <- getWord32le
    itemData <- getByteString (fromEnum itemSize)
    let (keyData, valueData) = BSC.span (/= separator) itemData
        key = Enc.decodeStrictByteString ASCII keyData
        value = Enc.decodeStrictByteString UTF8 (BS.tail valueData)
    return (VorbisItem key value)

-- | Writes a Vorbis item to a specified handle.
writeItem :: Handle -> VorbisItem -> IO ()
writeItem handle item =
    let itemData = runPut (putVorbisItem item)
    in BS.hPut handle (BSL.toStrict itemData)

-- | Reads a Vorbis item from the specified handle.
readItem :: Handle -> IO VorbisItem
readItem handle = do
        -- Read the item size.
        itemSize <- read32LE handle
        debugM "VorbisItem.readItem" ("Item size: " ++ show itemSize)
        when (itemSize > 65536)
            (warningM "VorbisItem.readItem"
                ("Item size (" ++ show itemSize ++ ") is probably invalid."))
        -- Read the item data.
        itemData <- BSC.hGet handle (fromEnum itemSize)
        let (key, value) = BSC.break (== separator) itemData
            k = BSC.unpack key
            v = BSC.unpack (BSC.tail value) in
            return (VorbisItem k v)