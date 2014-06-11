-- | A module for reading binary data in the specified endian
-- format. This module uses and supplements the ByteString library for
-- binary IO of integers.
--
-- The get family of functions use an existing buffer for the data, thus
-- avoiding an allocation per read. These functions can be used if performance
-- is important.
module Taglib.BinaryInput where

import Control.Exception(throwIO)
import Control.Monad(when)
import Data.Bits
import Data.Word
import Data.Char
import System.IO
import Data.Array.IO
import qualified Data.ByteString as BS

import Taglib.Exceptions
import Taglib.BinaryIOCommon

-- | Combines a low-order and a high-order byte into a word.
getBytes16 :: (Word8, Word8) -> Word16
getBytes16 (w1, w2) =
    toEnum word
    where
        word = w1i + shiftL w2i 8
        w1i = fromEnum w1
        w2i = fromEnum w2

-- | Combines four bytes into a double word.
getBytes32 :: (Word8, Word8, Word8, Word8) -> Word32
getBytes32 (dw1, dw2, dw3, dw4) =
    dword
    where
        dword :: Word32
        dword = dw1i + shiftL dw2i 8 + shiftL dw3i 16 + shiftL dw4i 24
        dw1i, dw2i, dw3i, dw4i :: Word32
        dw1i = fromIntegral dw1
        dw2i = fromIntegral dw2
        dw3i = fromIntegral dw3
        dw4i = fromIntegral dw4

-- | Extracts a double word from a specified index of a byte array.
--
-- The bytes are assumed to be in little endian order.
get32LE :: Buffer -- ^ Byte array whose length must be at least @i + 4@.
    -> Int -- ^ Starting index of the array to start reading from.
    -> IO Word32 -- ^ The four bytes combined into a double word.

get32LE buf i = do
    bytes <- (readDWordFromArray buf i)
    return (getBytes32 bytes)

-- | Extracts a double word from a specified index of a byte array.
--
-- The bytes are assumed to be in big endian order.
get32BE :: Buffer -- ^ Byte array whose length must be at least @i + 4@.
    -> Int -- ^ Starting index of the array to start reading from.
    -> IO Word32 -- ^ The four bytes combined into a double word.

get32BE buf i = do
    (b1, b2, b3, b4) <- (readDWordFromArray buf i)
    return (getBytes32 (b4, b3, b2, b1))

-- | Extracts a word from a specified index of a byte array.
--
-- The bytes are assumed to be in little endian order.
get16LE :: Buffer -> Int -> IO Word16
get16LE buf i = do
    b1 <- readArray buf i
    b2 <- readArray buf (i + 1)
    return (getBytes16 (b1, b2))

-- | Extracts a word from a specified index of a byte array.
--
-- The bytes are assumed to be in big endian order.
get16BE :: Buffer -> Int -> IO Word16
get16BE buf i = do
    b1 <- readArray buf i
    b2 <- readArray buf (i + 1)
    return (getBytes16 (b2, b1))

-- | Reads a sigle byte (represented by 'Word8') from the given handle.
readWord8 :: Handle -> IO Word8
readWord8 handle = do
    c <- hGetChar handle
    return (toEnum (ord c))

-- | Reads a word in little endian byte order from the given handle.
read16LE :: Handle -> IO Word16
read16LE handle = do
    arr <- newArray_ (0, 2) :: IO Buffer
    bytesRead <- hGetArray handle arr 2
    when (bytesRead /= 2) (ioError (userError "Could not read enough bytes from input"))
    get16LE arr 0

-- | Reads a word in big endian byte order from the given handle.
read16BE :: Handle -> IO Word16
read16BE handle = do
    arr <- newArray_ (0, 2) :: IO Buffer
    readToBuffer arr handle 2
    rev2 arr 0
    get16LE arr 0

-- | Reads a double word in little endian byte order from the given handle.
read32LE :: Handle -> IO Word32
read32LE handle = do
    arr <- newArray_ (0, 4) :: IO Buffer
    readToBuffer arr handle 2
    get32LE arr 0

-- | Reads a double word in big endian byte order from the given handle.
read32BE :: Handle -> IO Word32
read32BE handle = do
    arr <- newArray_ (0, 4) :: IO Buffer
    readToBuffer arr handle 4
    rev4 arr 0
    get32LE arr 0    

-- | Reads the specified number of bytes from the given handle.
readData :: Handle -> Int -> IO [Word8]
readData handle count = do
    bytes <- BS.hGet handle count
    return (BS.unpack bytes)
    
-- | Reads four bytes (double word) from a specified index of a byte array.
readDWordFromArray :: Buffer -> Int -> IO (Word8, Word8, Word8, Word8)
readDWordFromArray buf i = do
    b1 <- readArray buf i
    b2 <- readArray buf (i + 1)
    b3 <- readArray buf (i + 2)
    b4 <- readArray buf (i + 3)
    return (b1, b2, b3, b4)
    
-- | Reads exactly the specified amount of bytes into a buffer from a handle.
--
-- Raises a TaglibIOException if the read failed.
readToBuffer :: Buffer -> Handle -> Int -> IO ()
readToBuffer buf handle n = do
    bytesRead <- hGetArray handle buf n
    when (bytesRead /= n) (throwIO (TaglibIOException "Could not read enough bytes from input"))
    return ()
