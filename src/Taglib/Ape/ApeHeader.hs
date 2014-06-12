-- | Functions and data types for handing APE tag headers.
--
-- The header contains information about the tag, such as the number of items,
-- size etc.
--
-- The header is constructed as follows:
--
-- [@8 bytes@] Preamble @APETAGEX@
--
-- [@4 bytes@] Tag version
--
-- [@4 bytes@] Tag size in bytes, excluding the header size
--
-- [@4 bytes@] Number of items in the tag
--
-- [@4 bytes@] Global flags
--
-- [@8 bytes@] Eight zero bytes
--
-- The APEv2 tag flags specify whether the tag is read-only,
-- has a header, has a footer and whether this header instance is a header
-- or a footer.
module Taglib.Ape.ApeHeader(ApeHeader(..),
                             headerSize,
                             HeaderFlags(..),
                             ApeTagVersion(..),
                             apeV1VersionCode,
                             apeV2VersionCode,
                             toVersionCode,
                             toApeVersion,
                             toHeader,
                             toFooter,
                             writeHeader,
                             readHeader) where

import qualified Data.ByteString.Lazy as BS
import Control.Exception(throw)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.List(delete)
import Data.Word
import System.IO

import Taglib.Exceptions
import Taglib.Signatures

-- | The APE header data type.
data ApeHeader
    -- | Constructs an 'ApeHeader' with specified version code, tag size,
    -- item count and flags.
    = ApeHeader { tagVersion :: ApeTagVersion,
                  tagSize :: Int,
                  itemCount :: Int,
                  headerFlags :: [HeaderFlags] } deriving Eq
 
-- | APEv2 header flags.
data HeaderFlags =
    -- | Contents of the Ape tag are meant to be
    -- read-only.
    ReadOnly | 
    -- APE header is a header
    -- that precedes the APE tag data and not a footer that is placed after
    -- the data.
    IsHeader |
    -- APE tag uses an APE header
    -- placed before the APE tag data. This is necessary for streaming.
    HasHeader |
    -- APE tag uses an APE footer
    -- placed after the APE tag data. This is the default behaviour with APEv1.
    HasFooter
    deriving (Eq, Show)

instance Show ApeHeader where
    show hdr =
        concat [ "APE Tag Version : ",
                 show (tagVersion hdr),
                 "\nTag size        : ",
                 show (tagSize hdr),
                 "\nNumber of items : ",
                 show (itemCount hdr),
                 "\nFlags           : ",
                 show (headerFlags hdr) ]

-- | High-level representation of APE tag version.
data ApeTagVersion
    = ApeV1 -- ^ APE tag version 1.
    | ApeV2 -- ^ APE tag version 2.
    deriving (Eq, Ord, Show)

readOnlyMask, isHeaderMask, noFooterMask, hasHeaderMask :: Word32
readOnlyMask = bit 0
isHeaderMask = bit 29
noFooterMask = bit 30
hasHeaderMask = bit 31

-- | Integer version code for APEv1.
apeV1VersionCode :: Int
apeV1VersionCode = 1000

-- | Integer version code for APEv2.
apeV2VersionCode :: Int
apeV2VersionCode = 2000

-- | Gets the corresponding integer version code from an 'ApeTagVersion'.
toVersionCode :: ApeTagVersion -> Int
toVersionCode ApeV1 = apeV1VersionCode
toVersionCode ApeV2 = apeV2VersionCode

-- | The total size in bytes of an APE header.
headerSize :: Int
headerSize = 32

flagsToWord32 :: [HeaderFlags] -> Word32
flagsToWord32 flags = worker startVal flags where
    startVal = if notElem HasFooter flags then noFooterMask else 0
    worker n [] = n
    worker n (f : fs) = worker (n .|. flag) fs
        where
            flag = case f of
                    ReadOnly -> readOnlyMask
                    IsHeader -> isHeaderMask
                    HasHeader -> hasHeaderMask
                    HasFooter -> 0

word32ToFlags :: Word32 -> [HeaderFlags]
word32ToFlags flags =
    if hasHeader flags
        then HasHeader : worker flags
        else worker (flags .&. complement noFooterMask)
    where
        worker w
            | w == 0 = []
            | otherwise =
                if isReadOnly w then ReadOnly : worker (w .&. complement readOnlyMask)
                else if isHeader w then IsHeader : worker (w .&. complement isHeaderMask)
                else if hasHeader w then HasHeader : worker (w .&. complement hasHeaderMask)
                else worker (w .&. complement readOnlyMask)

isReadOnly :: Word32 -> Bool
isHeader :: Word32 -> Bool
hasHeader :: Word32 -> Bool
hasFooter :: Word32 -> Bool

isReadOnly f = (f .&. readOnlyMask) /= 0
isHeader f = (f .&. isHeaderMask) /= 0
hasHeader f = (f .&. hasHeaderMask) /= 0
hasFooter f = (f .&. noFooterMask) == 0

-- | Gets an APE tag version from the version code or errors if the version
-- code is unknown.
toApeVersion :: Int -> ApeTagVersion
toApeVersion ver
    | ver == apeV1VersionCode = ApeV1
    | ver == apeV2VersionCode = ApeV2
    | otherwise = throw (TaglibFormatException "Unknown APE tag version")

-- | Gets the given APE header with the 'isHeader' bit set to one in the
-- header flags.
toHeader :: ApeHeader -> ApeHeader
toHeader hdr =
    hdr { headerFlags = newFlags }
    where
        newFlags = if IsHeader `notElem` flags
            then IsHeader : flags
            else flags
        flags = headerFlags hdr

-- | Gets the given APE header with the 'isHeader' bit set to zero in the
-- header flags.
toFooter :: ApeHeader -> ApeHeader
toFooter hdr =
    hdr { headerFlags = newFlags }
    where newFlags = delete IsHeader (headerFlags hdr)
        
-- | Writes the header into the specified file handle.
writeHeader :: Handle -> ApeHeader -> IO ()
writeHeader handle header = BS.hPut handle headerData
    where headerData = runPut (putApeHeader header)

readHeader :: Handle -- ^ The file handle. The file must be
                     -- positioned to the beginning of the APE header before
                     -- calling this function.
    -> IO ApeHeader -- ^ The read APE header.
readHeader handle = do
    bs <- BS.hGet handle headerSize
    return (readFromByteString bs)

readFromByteString :: BS.ByteString -> ApeHeader
readFromByteString bs =
    let sig = BS.take 8 bs 
        headerData = BS.drop 8 (BS.take (toEnum headerSize) bs) in
    if BS.unpack sig == sigApe
        then runGet getApeHeader headerData
        else throw (TaglibInvalidKeyException "Invalid APE signature")

getApeHeader :: Get ApeHeader
getApeHeader = do
    ver <- getWord32le
    size <- getWord32le
    count <- getWord32le
    flags <- getWord32le
    skip 8
    return $! ApeHeader
        (toApeVersion (fromEnum ver))
        (fromEnum size)
        (fromEnum count)
        (word32ToFlags flags)

putApeHeader :: ApeHeader -> Put
putApeHeader header = do
    putLazyByteString (BS.pack sigApe)
    putWord32le (toEnum (toVersionCode $ tagVersion header))
    putWord32le (toEnum (tagSize header))
    putWord32le (toEnum (itemCount header))
    putWord32le (flagsToWord32 $ headerFlags header)
    putWord64le 0
