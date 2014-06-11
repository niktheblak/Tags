-- | Functions for dealing with ID3v2 headers.
--
-- The ID3v2 header flags consists of one byte with bits 4-7 containing flags:
--
-- [@Bit 4@] The ID3v2 tag uses a footer in addition to a header.
--
-- [@Bit 5@] The ID3v2 tag uses some experimental features or is of
-- experimental version.
--
-- [@Bit 6@] The ID3v2 tag uses an extended header.
--
-- [@Bit 7@] The ID3v2 tag uses the unsyncronization scheme.
--
-- The 'makeFlags' function can be used to generate ID3v2 header flags based
-- on desired parameters. The 'footer', 'experimental', 'extendedHeader' and
-- 'unsync' functions can be used to examine whether a given flag is set in
-- the header flags.
module Taglib.Id3v2.Header(Header(..),
                           HeaderFlags(..),
                           readHeader,
                           writeHeader) where

import Data.Bits
import Data.Word
import System.IO

import Taglib.BinaryIO
import Taglib.Id3v2.SynchSafe

-- | ID3v2 tag header. The header contains the following information:
--
-- [@8 bits@] Major version identifier.
--
-- [@8 bits@] Minor version identifier.
--
-- [@16 bits@] Flags.
--
-- [@32 bits@] Synchsafe encoded ID3v2 tag size.
data Header =
    -- | Constructs an ID3v2 header with specified data.
    Header { majorVersion :: Int,
             minorVersion :: Int,
             flags :: [HeaderFlags],
             size :: Int } deriving Eq

instance Show Header where
    show hdr =
        concat [ "Major version: ",
                  show (majorVersion hdr),
                  "\nMinor version: ",
                  show (minorVersion hdr),
                  "\nFlags        : ",
                  show (flags hdr),
                  "\nTag size     : ",
                  show (size hdr) ]

data SerializableHeader = Hdr !Word8 !Word8 !Word16 !Word32 deriving Eq

-- | ID3v2 header flags.
data HeaderFlags =
    Footer | 
    Experimental |
    ExtendedHeader |
    Unsync deriving (Eq, Show)

noneMask, footerMask, expMask, extHdrMask, unsyncMask :: Word16

noneMask = 0
footerMask = bit 5
expMask = bit 6
extHdrMask = bit 7
unsyncMask =  bit 8

{- showFlags :: Id3v2Flags -> String
showFlags f =
    concat [ "Flags: ",
             if footer f then "[footer] " else "",
             if experimental f then "[experimental] " else "",
             if extendedHeader f then "[extended header] " else "",
             if unsync f then "[unsync] " else "" ] -}

footer :: Word16 -> Bool
footer f = (f .&. footerMask) /= 0

experimental :: Word16 -> Bool
experimental f = (f .&. expMask) /= 0

extendedHeader :: Word16 -> Bool
extendedHeader f = (f .&. extHdrMask) /= 0

unsync :: Word16 -> Bool
unsync f = (f .&. unsyncMask) /= 0

setB :: (Bits a) => a -> a -> Bool -> a
setB mask w b = if b then w .|. mask else w .&. complement mask

flagsToWord16 :: [HeaderFlags] -> Word16
flagsToWord16 flags = worker 0 flags where
    worker n [] = n
    worker n (f : fs) = worker (n .|. x) fs
        where x = case f of
                Footer -> footerMask
                Experimental -> expMask
                ExtendedHeader -> extHdrMask
                Unsync -> unsyncMask

word16ToFlags :: Word16 -> [HeaderFlags]
word16ToFlags flags = worker flags
    where
        worker 0 = []
        worker w =
            if footer w then Footer : worker (w .&. complement footerMask)
            else if experimental w then Experimental : worker (w .&. complement expMask)
            else if extendedHeader w then ExtendedHeader : worker (w .&. complement extHdrMask)
            else if unsync w then Unsync : worker (w .&. complement unsyncMask)
            else []

toSerializable :: Header -> SerializableHeader
toSerializable hdr = Hdr maver miver fl sz
    where
        maver = toEnum (majorVersion hdr)
        miver = toEnum (minorVersion hdr)
        fl = flagsToWord16 $ flags hdr
        sz = toEnum (size hdr)

fromSerializable :: SerializableHeader -> Header
fromSerializable (Hdr maver miver fl sz) =
    Header { majorVersion = maver',
             minorVersion = miver',
             flags = fl',
             size = sz' }
    where
        maver' = fromEnum maver
        miver' = fromEnum miver
        fl' = word16ToFlags fl
        sz' = fromEnum sz
        
-- | Reads an ID3v2 header from the specified handle.
readHeader :: Handle -> IO Header
readHeader handle = do
    maver <- readWord8 handle
    miver <- readWord8 handle
    fl <- read16LE handle
    sz_ss <- read32LE handle
    let hdr = Hdr maver miver fl (fromSynchSafe sz_ss)
    return (fromSerializable hdr)

-- | Writes an ID3v2 header to the specified handle.
writeHeader :: Handle -> Header -> IO ()
writeHeader handle hdr =
    let (Hdr maver miver fl sz) = toSerializable hdr
    in do
        writeWord8 handle maver
        writeWord8 handle miver
        write16LE handle fl
        write32LE handle (toSynchSafe sz)
