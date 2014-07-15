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
module Tags.Vorbis.VorbisComment(VorbisComment,
                                 defaultVendor,
                                 toVorbisComment,
                                 toVorbisComment',
                                 toList,
                                 toList',
                                 writeVorbisComment,
                                 readVorbisComment) where

import Control.Monad(when)
import Data.Array.IO
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import System.IO
import System.Log.Logger

import Tags.BinaryIO
import Tags.ItemData
import Tags.StringUtils
import Tags.Vorbis.VorbisItem

-- | A Vorbis comment type is defined as a list of 'VorbisItem's.
data VorbisComment = VorbisComment String [VorbisItem]

defaultVendor :: String
defaultVendor = "Xiph.Org libVorbis I 20020717"

-- | Converts a list of 'TagItem' instances into a 'VorbisComment'.
--
-- Items whose keys are not valid Vorbis item keys or whose values are not
-- of type 'Text' are discarded.
toVorbisComment :: (TagItem a) => [a] -> VorbisComment
toVorbisComment items =
    VorbisComment defaultVendor vorbisItems
    where
        vorbisItems = map toVorbis validItems 
        validItems = filter isValid items
        isValid i = isValidKey (key i) && isTextual (value i)
        toVorbis i = case value i of
            Text t -> createVorbisItem (key i) t
            _ -> error "Non-textual Vorbis item."

-- | Converts a list of Key - 'HasItemData' pairs into a 'VorbisComment'.
--
-- Items whose keys are not valid Vorbis item keys or whose values are not
-- of type 'Text' are discarded.
toVorbisComment' :: (HasItemValue a) => [(String, a)] -> VorbisComment
toVorbisComment' items =
    VorbisComment defaultVendor vorbisItems
    where
        vorbisItems = map toVorbis validItems
        isValid (k, v) =
            isValidKey k && isTextual (itemValue v)
        validItems = filter isValid items
        toVorbis (k, v) = case itemValue v of
            Text t -> createVorbisItem k t
            _ -> error "Non-textual Vorbis item."

-- | Generates a list of Vorbis items from the given map of textual keys to
-- values.
--
-- Pairs whose keys are not valid Vorbis item keys are discarded.
toList :: Map.Map String String -> VorbisComment
toList mp =
    VorbisComment defaultVendor vorbisItems
    where
        vorbisItems = [createVorbisItem k v | (k, v) <- validItems]
        validItems = filter isValid (Map.toList mp)
        isValid (k, _) = isValidKey k

-- | Generates a list of Vorbis items from the given map of keys to
-- 'HasItemData' instances.
--
-- Pairs whose keys are not valid Vorbis item keys or whose values are not
-- of type 'Text' are discarded.
toList' :: (HasItemValue a) => Map.Map String a -> VorbisComment
toList' mp = toVorbisComment' (Map.toList mp)

getVorbisItems :: Int -> Get [VorbisItem]
getVorbisItems 0 = return []
getVorbisItems n = do
    item <- getVorbisItem
    rest <- getVorbisItems (n - 1)
    return (item : rest)

getVorbisComment :: Get VorbisComment
getVorbisComment = do
    vendorSize <- getWord32le
    vendor <- getByteString (fromIntegral vendorSize)
    itemCount <- getWord32le
    items <- getVorbisItems (fromIntegral itemCount)
    sync <- getWord8
    when (sync /= 1) (fail "Invalid synchronization bit")
    return (VorbisComment (BSC.unpack vendor) items)

putVorbisItems :: [VorbisItem] -> Put
putVorbisItems [] = return ()
putVorbisItems (item : items) = do
    putVorbisItem item
    putVorbisItems items

putVorbisComment :: VorbisComment -> Put
putVorbisComment (VorbisComment vendor items) =
    let vendorData = BSC.pack vendor
        vendorSize = BSC.length vendorData
    in do
        putWord32le (fromIntegral vendorSize)
        putByteString vendorData
        putWord32le (fromIntegral (length items))
        putVorbisItems items
        putWord8 1

-- | Writes a Vorbis comment into a specified handle using the specified
-- vendor string.
writeVorbisComment :: Handle -- ^ The handle the Vorbis comment is written to.
    -> VorbisComment -- ^ Vorbis comment.
    -> IO ()
writeVorbisComment handle vorbisComment =
    let vorbisCommentData = runPut (putVorbisComment vorbisComment)
    in BSL.hPut handle vorbisCommentData

-- | Reads a Vorbis comment from the specified handle. Returns the vendor
-- string and the Vorbis items.
readVorbisComment :: Handle -> IO VorbisComment
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
        vendor <- BSC.hGet handle (fromEnum vendorLength)
        debugM "VorbisComment.readVorbisComment"
            ("Vendor string: " ++ getPrintables (BSC.unpack vendor))
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
        return (VorbisComment (BSC.unpack vendor) items)

-- | Reads a Vorbis comment from the specified handle. Returns the vendor
-- string and the Vorbis items.
readVorbisComment' :: Handle -> IO VorbisComment
readVorbisComment' handle = do
    content <- BSL.hGetContents handle
    return $ runGet getVorbisComment content
