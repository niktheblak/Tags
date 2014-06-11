module Taglib.Id3v2.ExtendedHeader(ExtHeader(..),
                                    Restriction(..),
                                    extHeaderSize,
                                    writeExtHeader,
                                    readExtHeader) where

import Data.Bits
import Data.List(sort)
import Data.Word
import System.IO

import Taglib.BinaryIO
import Taglib.Id3v2.SynchSafe

-- | Defines an ID3v2 extended header. The extended header has the following
-- fields:
--
-- [@Update@] The tag is an update.
--
-- [@CRC32@] CRC32 checksum of the entire ID3v2 tag data.
--
-- [@Restrictions@] Tag restrictions, a list of 'Restriction' instances.
data ExtHeader = ExtHeader Bool (Maybe Word32) (Maybe [Restriction]) deriving Eq

instance Show ExtHeader where
    show (ExtHeader updt crc32 restr) = concat [
        "Update: ",
        show updt,
        case crc32 of
            Just c -> "\nCRC32 Checksum: " ++ show c
            Nothing -> "",
        case restr of
            Just r -> "\nRestrictions: " ++ show r
            Nothing -> "" ]

data ExtHeaderFlag = Update | CRC32 | Restricted deriving (Eq, Show)

-- | Defines ID3v2 tag restrictions. These restrictions should be enforced
-- by tag writing software if they are used.
data Restriction =
    -- | All images are 256x256 pixels or smaller.
    ImagesUnder256 |
    -- | All images are 64x64 pixels or smaller.
    ImagesUnder64 |
    -- | All images are exactly 64x64 pixels, unless required otherwise.
    ImagesExactly64 |
    -- | Images are encoded only with PNG or JPEG.
    ImagesPNGOrJPEG |
    -- | No string is longer than 1024 characters.
    TextUnder1024 |
    -- | No string is longer than 128 characters.
    TextUnder128 |
    -- | No string is longer than 30 characters.
    TextUnder30 |
    -- | Strings are only encoded with ISO-8859-1 or UTF-8.
    EightBitStrings |
    -- | No more than 128 frames and 1 MB total tag size.
    TagSizeUnder1024 |
    -- | No more than 64 frames and 128 KB total tag size.
    TagSizeUnder128 |
    -- | No more than 32 frames and 40 KB total tag size.
    TagSizeUnder40 |
    -- | No more than 32 frames and 4 KB total tag size.
    TagSizeUnder4
    deriving (Eq, Show, Ord)
    
getupd (ExtHeader updt _ _) = if updt then [Update] else []

getFlags :: ExtHeader -> [ExtHeaderFlag]
getFlags hdr@(ExtHeader updt (Just _) (Just _)) = getupd hdr ++ [CRC32, Restricted]
getFlags hdr@(ExtHeader updt (Just _) Nothing) = getupd hdr ++ [CRC32]
getFlags hdr@(ExtHeader updt Nothing (Just _)) = getupd hdr ++ [Restricted]
getFlags hdr@(ExtHeader updt Nothing Nothing) = getupd hdr ++ [Update]

flagsToWord8 :: [ExtHeaderFlag] -> Word8
flagsToWord8 flags = worker 0 flags where
    worker n [] = n
    worker n (f : fs) = worker (n .|. x) fs
        where x = case f of
                    Update -> 0x10
                    CRC32 -> 0x20
                    Restricted -> 0x40

word8ToFlags :: Word8 -> [ExtHeaderFlag]
word8ToFlags w = concat [
    if (w .&. 0x10) /= 0 then [Update] else [],
    if (w .&. 0x20) /= 0 then [CRC32] else [],
    if (w .&. 0x40) /= 0 then [Restricted] else [] ]

rToWord8 :: [Restriction] -> Word8
rToWord8 restr = worker 0 restr where
    worker n [] = n
    worker n (r : rs) = worker (n .|. x) rs
        where x = case r of
                    ImagesUnder256 -> imagesUnder256Mask
                    ImagesUnder64 -> imagesUnder64Mask
                    ImagesExactly64 -> imagesExactly64Mask
                    ImagesPNGOrJPEG -> imagesPNGOrJPEGMask
                    TextUnder1024 -> textUnder1024Mask
                    TextUnder128 -> textUnder128Mask
                    TextUnder30 -> textUnder30Mask
                    EightBitStrings -> eightBitStringsMask
                    TagSizeUnder128 -> tagSizeUnder128Mask
                    TagSizeUnder40 -> tagSizeUnder40Mask
                    TagSizeUnder4 -> tagSizeUnder4Mask
                    otherwise -> 0

word8ToR :: Word8 -> [Restriction]
word8ToR restr = sort (worker restr) where
    worker 0 = []
    worker r =
        if (r .&. imagesExactly64Mask) == imagesExactly64Mask
        then ImagesExactly64 : worker (r .&. complement imagesExactly64Mask)
        else
        if (r .&. imagesUnder256Mask) /= 0
        then ImagesUnder256 : worker (r .&. complement imagesUnder256Mask)
        else
        if (r .&. imagesUnder64Mask) /= 0
        then ImagesUnder64 : worker (r .&. complement imagesUnder64Mask)
        else
        if (r .&. imagesPNGOrJPEGMask) /= 0
        then ImagesPNGOrJPEG : worker (r .&. complement imagesPNGOrJPEGMask)
        else
        if (r .&. textUnder30Mask) == textUnder30Mask
        then TextUnder30 : worker (r .&. complement textUnder30Mask)
        else
        if (r .&. textUnder1024Mask) /= 0
        then TextUnder1024 : worker (r .&. complement textUnder1024Mask)
        else
        if (r .&. textUnder128Mask) /= 0
        then TextUnder128 : worker (r .&. complement textUnder128Mask)
        else
        if (r .&. eightBitStringsMask) /= 0
        then EightBitStrings : worker (r .&. complement eightBitStringsMask)
        else
        if (r .&. tagSizeUnder4Mask) == tagSizeUnder4Mask
        then TagSizeUnder4 : worker (r .&. complement tagSizeUnder4Mask)
        else
        if (r .&. tagSizeUnder128Mask) /= 0
        then TagSizeUnder128 : worker (r .&. complement tagSizeUnder128Mask)
        else
        if (r .&. tagSizeUnder40Mask) /= 0
        then TagSizeUnder40 : worker (r .&. complement tagSizeUnder40Mask)
        else error "Unrecognized flag."

-- | Gets the size in bytes of an extended header.
extHeaderSize :: ExtHeader -> Word32
extHeaderSize (ExtHeader updt crc32 restr) =
    6 + (case crc32 of
            Just _ -> 5
            Nothing -> 0) +
        (case restr of
            Just _ -> 1
            Nothing -> 0)

-- | Writes an ID3v2 extended header to the specified handle.
writeExtHeader :: Handle -> ExtHeader -> IO ()
writeExtHeader handle hdr@(ExtHeader updt crc32 restr) =
    let hdrSize = extHeaderSize hdr
        hdrSize_ss = toSynchSafe hdrSize
        flags = flagsToWord8 $ getFlags hdr
    in do
        write32LE handle hdrSize_ss
        writeWord8 handle 1
        writeWord8 handle flags
        case crc32 of
            Just c -> write32LE handle (toSynchSafe c)
            Nothing -> return ()
        case restr of
            Just r -> writeWord8 handle (rToWord8 r)
            Nothing -> return ()
        
-- | Reads an ID3v2 extended header from the specified handle.
readExtHeader :: Handle -> IO ExtHeader
readExtHeader handle = do
    hdrSize_ss <- read32LE handle
    let hdrSize = fromSynchSafe hdrSize_ss
    flagCount <- readWord8 handle
    flags_w <- readWord8 handle
    let flags = word8ToFlags flags_w
    crc32 <- if elem CRC32 flags
                then do
                    crc32_ss <- read32LE handle
                    let crc32 = fromSynchSafe crc32_ss
                    return (Just crc32)
                else return Nothing
    restr <- if elem Restricted flags
                then do
                    restr_w8 <- readWord8 handle
                    let restr = word8ToR restr_w8
                    return (Just restr)
                else return Nothing
    let updt = elem Update flags
    return (ExtHeader updt crc32 restr)

imagesUnder256Mask, imagesUnder64Mask, imagesExactly64Mask, imagesPNGOrJPEGMask,
    textUnder1024Mask, textUnder128Mask, textUnder30Mask, eightBitStringsMask,
    tagSizeUnder128Mask, tagSizeUnder40Mask, tagSizeUnder4Mask :: Word8

imagesUnder256Mask = 1
imagesUnder64Mask = 2
imagesExactly64Mask = 1 .|. 2 -- 3
imagesPNGOrJPEGMask = 4
textUnder1024Mask = 8
textUnder128Mask = 16
textUnder30Mask = 8 .|. 16 -- 24
eightBitStringsMask = 32
tagSizeUnder128Mask = 64
tagSizeUnder40Mask = 128
tagSizeUnder4Mask = 64 .|. 128 -- 192
