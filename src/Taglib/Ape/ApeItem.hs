-- | Represents an APE tag item.
--
-- An APE Tag item consists of the following elements:
--
-- * ASCII-encoded key
--
-- * Item value, can contain either UTF-8-encoded text, binary data, or
--   a locator to external data
--
-- * Item-specific APE flags
--    
-- The key's length must be between 2 and 255 characters, and
-- the allowed character ranges are ASCII @0x20@ (spacebar) to
-- @0x7F@ (tilde) inclusive. The function 'isValidKey'
-- checks if a given text string is a valid APE item key.
--
-- Item data can be anything; currently it can contain UTF-8-encoded
-- text, binary data or a locator to external data. The item data's type
-- is specified in the item flags.
--
-- The item flags specify the type of the ape item from the three options
-- text, binary or locator. The item flags also specify whether an APE item
-- is read-only. The definition of a /read-only item/ is very vague and
-- the exact interpretation of the feature is basically left for the
-- implementation.
--
-- The locator to external data is implemented by using the 'Network.URI.URI'
-- data type from the Haskell library. Technically the APEv2 specification
-- doesn't mandate using the RFC2396 standard URI encoding for the locator
-- data type but I see no harm done since RFC2396 is stricter
-- than the overly simplified locator scheme described in the APEv2
-- specification.
module Taglib.Ape.ApeItem(minKeyLength,
                           maxKeyLength,
                           ApeItemValue(..),
                           ApeItem(..),
                           Key,
                           isValidKey,
                           itemSize,
                           writeItem,
                           writeItem',
                           readItem,
                           readItems,
                           toApeItemValue,
                           toApeItem,
                           compareSize) where

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString as BS
import Control.Exception(throw, throwIO)
import Data.Array.IO
import Data.Word
import Data.Bits
import Data.Char
import Network.URI(URI(..),parseURIReference)
import System.IO
import System.Log.Logger

import Taglib.Exceptions
import Taglib.ItemData
import Taglib.Ascii
import Taglib.BinaryIO

-- | The minimum length for an APE item key.
minKeyLength :: Int
minKeyLength = 2

-- | The maximum length for an APE item key.
maxKeyLength :: Int
maxKeyLength = 255

utf8Mask, readOnlyMask, locatorMask, binaryMask, reservedMask :: Word32

utf8Mask = 0
readOnlyMask = 1
locatorMask = 2
binaryMask = 4
reservedMask = locatorMask .|. binaryMask

-- | Type for APE item flags.
type ItemFlags = Word32

-- | APE item data types.
data ApeItemValue = ApeText String -- ^ UTF-8 text.
    | ApeBinary BS.ByteString -- ^ Binary Data.
    | ApeLocator URI -- ^ Locator to external data.
    | ApeReserved BS.ByteString -- ^ Reserved for future use.
    deriving Eq

-- | An APE item is defined as a key-value pair containing a textual key
-- and user-specified data type 'ApeItemData'.
--
-- An APE item can be constructed as such:
--
-- @item = Ape \"ARTIST\" (ApeText \"King Diamond\")@ or
--
-- @item = Ape \"COVER\" (ApeBinary [ 0, 1, 2, 3 ])@ or
--
-- >maybeUri = parseURIReference "http://www.covenworldwide.org"
-- >uri = case maybeUri of
-- >    Nothing -> error "Invalid URI."
-- >    Just u -> u
-- >item = Ape \"HOMEPAGE\" (ApeLocator uri)
--
-- It may seem too strict to require a valid URI instance for constructing
-- a 'ApeLocator' item type but it is the only way to ensure generation and
-- storage of only APEv2 specification conformant APE items.
data ApeItem = Ape Key ApeItemValue deriving Eq

-- | Type for APE item key.
--
-- The item key is an ASCII text string whose length must
-- be between 2 and 255 characters and contain only characters from range
-- @0x20@ (spacebar) to @0x7F@ (tilde) inclusive. The function 'isValidKey'
-- checks if a given text string is a valid APE item key.
type Key = String

instance Show ApeItem where
    show (Ape key value) = key ++ " = " ++ show value

instance Show ApeItemValue where
    show (ApeText t) = t
    show (ApeBinary b) = "[binary data " ++ show (BS.length b) ++ " bytes]"
    show (ApeLocator l) = show l
    show (ApeReserved r) = "[binary data " ++ show (BS.length r) ++ " bytes]"

instance HasItemValue ApeItemValue where
    itemValue (ApeText t) = Text t
    itemValue (ApeBinary b) = Binary b
    itemValue (ApeLocator l) = Text (show l)
    itemValue (ApeReserved r) = Binary r

instance TagItem ApeItem where
    key (Ape k _) = k
    value (Ape _ v) = itemValue v

typeToFlags :: ApeItemValue -> ItemFlags
typeToFlags (ApeText _) = utf8Mask
typeToFlags (ApeBinary _) = binaryMask
typeToFlags (ApeLocator _) = locatorMask
typeToFlags (ApeReserved _) = reservedMask

-- | Zeroes every bit except locator-bit and binary-bit from the given flag.
stripFlags :: ItemFlags -> ItemFlags
stripFlags f = f .&. reservedMask

isText, isBinary, isLocator, isReserved, isReadOnly :: ItemFlags -> Bool

isText f = stripFlags f == 0
isBinary f = (stripFlags f .&. binaryMask) /= 0
isLocator f = (stripFlags f .&. locatorMask) /= 0
isReserved f = (stripFlags f .&. reservedMask) /= 0
isReadOnly f = f .&. readOnlyMask /= 0

-- | Gets the integer APE item flags generated from 'ApeItemData' and
-- a read-only specifier.
getFlags :: ApeItemValue -- ^ The APE item data.
    -> Bool -- ^ Whether the item should be read-only.
    -> ItemFlags -- ^ The generated flags.
getFlags item readOnly =
    flags
    where
        flags = typeToFlags item .|. readOnlyFlag
        readOnlyFlag = if readOnly then readOnlyMask else 0

-- | Checks whether a given text string is a valid APE item key.
--
-- In order to be a valid APE item key, the string's length must
-- be between 2 and 255 characters and it must contain only character from range
-- @0x20@ to @0x7F@ inclusive.
isValidKey :: String -> Bool
isValidKey key
    | length key < minKeyLength = False
    | length key > maxKeyLength = False
    | otherwise = all isValid key
    where
        isValid :: Char -> Bool
        isValid c
            | ord c < 0x20 = False
            | ord c > 0x7F = False
            | otherwise = True

-- | Get the size in bytes a given APE item will take when serialized.
itemSize :: ApeItem -> Int
itemSize (Ape key value) =
    4 + 4 + length key + 1 + dataLength value where
        dataLength :: ApeItemValue -> Int
        dataLength (ApeText text) = length (UTF8.encode text)
        dataLength (ApeBinary bdata) = BS.length bdata
        dataLength (ApeLocator uri) = length (UTF8.encode (show uri))
        dataLength (ApeReserved bdata) = BS.length bdata

-- | Writes an APE item into the specified handle.
writeItem :: Handle -> ApeItem -> IO ()
writeItem handle (Ape key value) =
    writeItem' handle (Ape key value) False

-- | Writes an APE item with given flags into the given handle.
writeItem' :: Handle -> ApeItem -> Bool -> IO ()
writeItem' handle (Ape key value) readOnly
    | not (isValidKey key) = throw (TaglibInvalidKeyException ("Invalid APE item key: " ++ key))
    | otherwise = 
    let bdata = writeApeItemValue value
        dataLen = toEnum (BS.length bdata)
        keyData = toAscii key
        flags = getFlags value readOnly
    in do
        arr <- createBuffer 8
        put32LE arr 0 dataLen           -- Length of data
        put32LE arr 4 flags             -- Item flags
        hPutArray handle arr 8
        writeData handle keyData        -- Item key
        hPutChar handle '\0'            -- Zero terminator
        BS.hPut handle bdata            -- Item data

writeApeItemValue :: ApeItemValue -> BS.ByteString
writeApeItemValue (ApeText t) = BS.pack (UTF8.encode t)
writeApeItemValue (ApeBinary b) = b
writeApeItemValue (ApeLocator l) = BS.pack (UTF8.encode (show l))
writeApeItemValue (ApeReserved r) = r

-- | Reads and validates an APE item key from the specified handle.
--
-- Fails with @ioError@ in case of an IO error or of the APE item key is
-- malformed.
readKey :: Handle -> IO Key
readKey handle = readKeyWorker 0 where
    readKeyWorker count
        | count > maxKeyLength = throwIO (TaglibInvalidKeyException "Too long APE item key")
        | otherwise = do
            c <- hGetChar handle
            if c == '\0' then return []
                else do
                    cs <- readKeyWorker (count + 1)
                    return (c : cs)

-- | Reads an APE item from the given handle.
--
-- Fails with @ioError@ in case of an IO error or of the APE item is
-- malformed.
--
-- If the indicated item type is Locator and the item does not contain a valid
-- URI string, an item with 'ApeText' data type will be returned instead.
readItem :: Handle -> IO ApeItem
readItem handle = do
    infoM "ApeItem.readItem" "Reading APE item..."
    arr <- createBuffer 8
    readToBuffer arr handle 8
    dataLen <- get32LE arr 0
    flags <- get32LE arr 4
    key <- readKey handle
    rawItemData <- BS.hGet handle (fromEnum dataLen)
    case readApeItemValue rawItemData flags of
        Nothing -> throwIO (TaglibFormatException "Invalid APE item data")
        Just itemData -> do
            let item = Ape key itemData
            infoM "ApeItem.readItem"
                ("APE item read: " ++ show item)
            return item
                
readApeItemValue :: BS.ByteString -> ItemFlags -> Maybe ApeItemValue
readApeItemValue dt flags
    | isText flags = Just (ApeText (UTF8.decode (BS.unpack dt)))
    | isBinary flags = Just (ApeBinary dt)
    | isLocator flags =
        let text = UTF8.decode (BS.unpack dt)
            uri = parseURIReference text in
            Just (case uri of
                Nothing -> ApeText text
                Just u -> ApeLocator u)
    | isReserved flags = Just (ApeReserved dt)
    | otherwise = Nothing

-- | Reads n APE items from the given handle.
readItems :: Handle -> Int -> IO [ApeItem]
readItems _ 0 = return []
readItems handle count = do
    item <- readItem handle
    items <- readItems handle (count - 1)
    return (item : items)

-- | Coerces 'ItemValue' to 'ApeItemValue'.
--
-- Since text and binary type have a direct mapping in APE tag format, no
-- information is lost.
toApeItemValue ::ItemValue -> ApeItemValue
toApeItemValue (Text t) = ApeText t
toApeItemValue (Binary b) = ApeBinary b

-- | Coerces a 'TagItem' to 'ApeItem'.
--
-- The conversion is subject to APE item key restrictions.
-- Text and binary type have a direct mapping in APE tag format so no
-- information is lost.
toApeItem :: (TagItem a) => a -> Maybe ApeItem
toApeItem item
    | isValidKey (key item) =
        let k = key item
            v = case value item of
                    Text t -> ApeText t
                    Binary b -> ApeBinary b
        in Just (Ape k v)
    | otherwise = Nothing

-- | Gives an 'Ordering' of two 'ApeItem's based on their size.
compareSize :: ApeItem -> ApeItem -> Ordering
compareSize item1 item2
    | itemSize item1 < itemSize item2 = LT
    | itemSize item1 > itemSize item2 = GT
    | otherwise = EQ

