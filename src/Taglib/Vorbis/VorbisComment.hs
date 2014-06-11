-- | Vorbis comments store textual key-value pairs where the value is UTF-8
-- encoded text. The key is limited on length and content but the value
-- is not, as long as it is valid UTF-8 text.
--
-- Keys are stored honouring the case but for comparison and search purposes
-- the case is ignored. Vorbis comments can store multiple items with
-- similar keys. This is unlike most other tag formats.
--
-- The restrictions for the key are:
--
--   * One or more characters in length
--
--   * The key only contains ASCII values in range 0x20 to 0x7D excluding
--     0x3D (=)
module Taglib.Vorbis.VorbisComment(VorbisComment,
                                   defaultVendor,
                                   toVorbisItems,
                                   toVorbisItems',
                                   toList,
                                   toList',
                                   writeVorbisComment,
                                   writeVorbisComment',
                                   readVorbisComment) where

import qualified Data.Map as Map
import Control.Monad(when)
import System.IO
import Data.Array.IO
import System.Log.Logger

import Taglib.Ascii
import Taglib.BinaryIO
import Taglib.ItemData
import Taglib.StringUtils
import Taglib.Vorbis.VorbisItem

-- | A Vorbis comment type is defined as a list of 'VorbisItem's.
type VorbisComment = [VorbisItem]

defaultVendor :: String
defaultVendor = "Xiph.Org libVorbis I 20020717"

-- | Converts a list of 'TagItem' instances into a list of Vorbis items.
--
-- Items whose keys are not valid Vorbis item keys or whose values are not
-- of type 'Text' are discarded.
toVorbisItems :: (TagItem a) => [a] -> VorbisComment
toVorbisItems items =
    map toVorbis validItems
    where
        validItems = filter isValid items
        isValid i = isValidKey (key i) && isTextual (value i)
        toVorbis i = case value i of
            Text t -> createVorbisItem (key i) t
            _ -> error "Non-textual Vorbis item."

-- | Converts a list of Key - 'HasItemData' pairs into a list of Vorbis items.
--
-- Items whose keys are not valid Vorbis item keys or whose values are not
-- of type 'Text' are discarded.
toVorbisItems' :: (HasItemData a) => [(String, a)] -> VorbisComment
toVorbisItems' items =
    map toVorbis validItems
    where
        isValid (k, v) =
            isValidKey k && isTextual (itemData v)
        validItems = filter isValid items
        toVorbis (k, v) = case itemData v of
            Text t -> createVorbisItem k t
            _ -> error "Non-textual Vorbis item."

-- | Generates a list of Vorbis items from the given map of textual keys to
-- values.
--
-- Pairs whose keys are not valid Vorbis item keys are discarded.
toList :: Map.Map String String -> VorbisComment
toList mp =
    [createVorbisItem k v | (k, v) <- validItems]
    where
        validItems = filter isValid (Map.toList mp)
        isValid (k, _) = isValidKey k

-- | Generates a list of Vorbis items from the given map of keys to
-- 'HasItemData' instances.
--
-- Pairs whose keys are not valid Vorbis item keys or whose values are not
-- of type 'Text' are discarded.
toList' :: (HasItemData a) => Map.Map String a -> VorbisComment
toList' mp = toVorbisItems' (Map.toList mp)

-- | Writes a Vorbis comment into a specified handle using the default vendor
-- string.
writeVorbisComment :: Handle -> VorbisComment -> IO ()
writeVorbisComment handle =
    writeVorbisComment' handle defaultVendor

-- | Writes a Vorbis comment into a specified handle using the specified
-- vendor string.
writeVorbisComment' :: Handle -- ^ The handle the Vorbis comment is written to.
    -> String -- ^ Vendor string.
    -> VorbisComment -- ^ Vorbis items.
    -> IO ()
writeVorbisComment' handle vendor items =
    let worker :: VorbisComment -> IO ()
        worker [] = return ()
        worker (i : is) = do
            writeItem handle i
            worker is
        vend = toAscii vendor
    in do
        infoM "VorbisComment.writeVorbisComment'" "Writing Vorbis comment..."
        arr <- newArray_ (0, 4) :: IO Buffer
        put32LE arr 0 (toEnum (length vend))  -- Vendor string length
        hPutArray handle arr 4
        writeData handle vend                   -- Vendor string
        put32LE arr 0 (toEnum (length items)) -- Number of items
        hPutArray handle arr 4
        worker items                            -- Items
        writeWord8 handle 1                     -- Synchronization bit
        hFlush handle
        infoM "VorbisComment.writeVorbisComment'" "Vorbis comment written."

-- | Reads a Vorbis comment from the specified handle. Returns the vendor
-- string and the Vorbis items.
readVorbisComment :: Handle -> IO (String, VorbisComment)
readVorbisComment handle =
    let readItems n
            | n == 0 = return []
            | otherwise = do
                item <- readItem handle
                items <- readItems (n - 1)
                return (item : items)
    in do
        infoM "VorbisComment.readVorbisComment" "Reading Vorbis comment..."
        arr <- createBuffer 4
        readToBuffer arr handle 4
        -- Read the vendor string length.
        vendorLength <- get32LE arr 0
        -- Read the vendor string.
        vendor <- readData handle (fromEnum vendorLength)
        debugM "VorbisComment.readVorbisComment"
            ("Vendor string: " ++ bytesToPrintableString vendor)
        -- Read the item count.
        readToBuffer arr handle 4
        itemCount <- get32LE arr 0
        debugM "VorbisComment.readVorbisComment"
            ("Number of items: " ++ show itemCount)
        when (itemCount > 65536)
            (warningM "VorbisComment.readVorbisComment"
                ("Indicated number of items (" ++ show itemCount ++ ") is probably invalid."))
        -- Read the Vorbis items.
        items <- readItems (fromEnum itemCount)
        -- Read the synchronization bit.
        sync <- readWord8 handle
        when (sync /= 1)
            (warningM "VorbisComment.readVorbisComment"
                "Erraneous synchronization bit.")
        infoM "VorbisComment.readVorbisComment" "Vorbis comment read."
        return (fromAscii vendor, items)
