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

import Control.Monad(when)
import Data.Bits
import Data.List(delete)
import Data.Word
import System.IO
import Data.Array.IO

import Taglib.Signatures
import Taglib.BinaryIO

-- | The APE header data type.
data ApeHeader
    -- | Constructs an 'ApeHeader' with specified version code, tag size,
    -- item count and flags.
    = ApeHeader { tagVersion :: ApeTagVersion,
                  tagSize :: Int,
                  itemCount :: Int,
                  headerFlags :: [HeaderFlags] } deriving Eq

-- | The APE header data type.
data SerializableApeHeader
    = Hdr !Word32 !Word32 !Word32 !Word32 deriving Eq
    
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
        worker 0 = []
        worker w =
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
    | otherwise = error "Unknown APE tag version."

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
writeHeader handle header =
    do
        writeData handle sigApe
        arr <- newArray_ (0, 16) :: IO Buffer
        let (Hdr ver ts ic fl) = toSerializable header
        put32LE arr 0 ver
        put32LE arr 4 ts
        put32LE arr 8 ic
        put32LE arr 12 fl
        hPutArray handle arr 16
        put32LE arr 0 0
        put32LE arr 4 0
        hPutArray handle arr 8

-- | Reads an APE header from the specified file handle or raises an ioError
-- if the file doesn't contain a valid APE header.
readHeader :: Handle -- ^ The file handle. The file must be readable but it
                     -- does not need to be seekable. The file must be
                     -- positioned to the beginning of the APE header before
                     -- calling this function.
    -> IO ApeHeader -- ^ The read APE header.
readHeader handle = do
    sig <- readData handle (length sigApe)
    when (sig /= sigApe) (ioError (userError "Invalid APE header."))
    arr <- newArray_ (0, 24) :: IO Buffer
    readToBuffer arr handle 24
    ver <- get32LE arr 0
    size <- get32LE arr 4
    count <- get32LE arr 8
    flags <- get32LE arr 12
    let hdr = Hdr ver size count flags
    return (fromSerializable hdr)

toSerializable :: ApeHeader -> SerializableApeHeader
toSerializable hdr = Hdr ver sz ic fl
    where
        ver = toEnum (toVersionCode $ tagVersion hdr)
        sz = toEnum (tagSize hdr)
        ic = toEnum (itemCount hdr)
        fl = flagsToWord32 $ headerFlags hdr

fromSerializable :: SerializableApeHeader -> ApeHeader
fromSerializable (Hdr v sz ic fl) =
    ApeHeader { tagVersion = ver,
                tagSize = size,
                itemCount = count,
                headerFlags = flags }
    where
        ver = toApeVersion (fromEnum v)
        size = fromEnum sz
        count = fromEnum ic
        flags = word32ToFlags fl
