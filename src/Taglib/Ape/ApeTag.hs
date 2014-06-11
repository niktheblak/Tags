-- | Functions for handling APE tags as a whole.
module Taglib.Ape.ApeTag where

import Control.Exception(assert)
import Control.Monad(when)
import Data.List(sortBy)
import qualified Data.Map as Map
import System.IO
import System.Log.Logger

import Taglib.ItemData
import Taglib.Ape.ApeHeader
import Taglib.Ape.ApeItem

-- | Gets the total size in bytes of the APE items.
getSize :: [ApeItem] -> Int
getSize items = sum [ itemSize i | i <- items ]

-- | Generates an APEv1 footer from the given APE items.
makeV1Footer :: [ApeItem] -> ApeHeader
makeV1Footer items =
    let size = getSize items
        count = length items
    in
    ApeHeader { tagVersion = ApeV1,
                tagSize = size,
                itemCount = count,
                headerFlags = [HasFooter] }

-- | Generates an APEv2 header from the given items and parameters.
makeV2Header :: [ApeItem] -- ^ The APE items.
    -> Bool -- ^ APEv2 tag uses a header.
    -> Bool -- ^ APEv2 tag uses a footer.
    -> ApeHeader

makeV2Header items useHeader useFooter =
    let size = getSize items + if useHeader then headerSize else 0
        count = length items
        flags = [HasFooter, HasHeader]
    in
    ApeHeader { tagVersion = ApeV2,
                tagSize = size,
                itemCount = count,
                headerFlags = flags }

-- | Converts a map of string keys and 'HasItemValue' instances
-- into a list of 'ApeItem's.
mapToApeItems :: (HasItemValue a) => Map.Map String a -> [ApeItem]
mapToApeItems mp =
    let pairToApe (k, val) = Ape k (toApeItemValue (itemValue val))
        items = Map.toList mp
        validItems = filter (\(k, _) -> isValidKey k) items
    in
    map pairToApe validItems

-- | Converts a map of string keys and 'ApeItemValue' values into
-- a list of 'ApeItem's.
mapToList :: Map.Map String ApeItemValue -> [ApeItem]
mapToList mp =
    let items = Map.toList mp
        validItems = filter (\(k, _) -> isValidKey k) items
    in
    [(Ape key value) | (key, value) <- validItems]

-- | Writes the given APE items as APEv1 tag.
writeApeV1Tag :: Handle -> [ApeItem] -> IO ()
writeApeV1Tag handle items =
    let hdr = makeV1Footer items in
    do
        infoM "ApeTag.writeApeV1Tag"
            ("Writing APEv1 tag with " ++ show (length items) ++ " items...")
        writeApeTag' handle items hdr

-- | Writes the given APE items as APEv2 tag with default APE tag flags.
--
--  Default APEv2 flags:
--
--   [@Tag has header@] @True@
--
--   [@Tag has footer@] @True@
--
--   [@This is header@] @True@
--
--   [@Tag is read-only@] @False@
writeApeV2Tag :: Handle -> [ApeItem] -> IO ()
writeApeV2Tag handle items =
    let hdr = makeV2Header items True True in do
    infoM "ApeTag.writeApeV2Tag"
        ("Writing APEv2 tag with " ++ show (length items) ++ " items with default flags...")
    writeApeTag' handle items hdr

-- | Writes an APE tag with options obtained from the given APE header.
--
-- The APE tag version and header\/footer\/readonly options are obtained
-- from the given APE header.
writeApeTag' :: Handle -- ^ The APE tag is written to this file handle.
    -> [ApeItem] -- ^ The APE tag items to write.
    -> ApeHeader -- ^ APE tag version and other options are obtained from this
                 -- header.
    -> IO ()

writeApeTag' handle items header =
    let writeItems [] = return ()
        writeItems (i : is) =
            do
                writeItem handle i
                writeItems is
        writeHeaderIf b hdr' =
            when b (do
                debugM "ApeTag.writeApeTag'"
                    ("Writing APE header:\n" ++ show hdr')
                writeHeader handle hdr')
        hdr = if tagVersion header == ApeV2 then toHeader header else header
        ftr = toFooter hdr
        flags = headerFlags hdr
        hasHdr = elem HasHeader flags
        hasFtr = elem HasFooter flags
        sortedItems = sortBy compareSize items
    in assert (hasHdr || hasFtr) (do
        -- Write the APE header
        writeHeaderIf hasHdr hdr
        debugM
            "ApeTag.writeApeTag'"
            "Writing APE tag items."
        -- Write the APE items
        writeItems sortedItems
        -- Write the APE footer
        writeHeaderIf hasFtr ftr
        infoM
            "ApeTag.writeApeTag'"
            "APE tag written."
        hFlush handle)

-- | Reads an APE tag (v1 or v2) from the given handle.
--
-- For APEv2 tags containing both a header or a footer the stream may be
-- positioned at the beginning of either the header or the footer.
-- For APEv1 tags the stream must be positioned to the beginning of the
-- footer and the stream must be seekable.
readApeTag :: Handle -- ^ The handle to read the APE tag from. Must be seekable
                     -- if reading an APEv1 tag.
    -> IO ([ApeItem], ApeHeader) -- ^ The APE items and the APE header.

readApeTag handle =
    let flags hdr = headerFlags hdr
        seekIf hdr =
            when (IsHeader `notElem` (flags hdr))
                (do
                    let seekSize = toInteger (fromEnum (tagSize hdr) + headerSize)
                    infoM "ApeTag.readApeTag"
                        ("Found a footer; seeking back " ++ show seekSize ++ " bytes...")
                    hSeek handle RelativeSeek (negate seekSize))
        skipFooterIf hdr =
            when (HasFooter `elem` (flags hdr))
                (hSeek handle RelativeSeek (toInteger headerSize))
    in do
        infoM "ApeTag.readApeTag" "Reading APE tag..."
        -- Read the APE header.
        hdr <- readHeader handle
        debugM "ApeTag.readApeTag" ("Read APE header:\n" ++ show hdr)
        -- Seek to the beginning of APE item data.
        seekIf hdr
        -- Read the APE items.
        items <- readItems handle (fromEnum (itemCount hdr))
        -- Skip the footer if it's present.
        skipFooterIf hdr
        infoM "ApeTag.readApeTag" "APE tag read complete."
        return (items, hdr)
