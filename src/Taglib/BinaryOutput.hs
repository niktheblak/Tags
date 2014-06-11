-- | A module for writing binary data in the specified endian
-- format. This module uses and supplements the ByteString library for
-- binary IO of integers.
--
-- The put family of functions use an existing buffer for the data, thus
-- avoiding an allocation per write. These functions can be used if performance
-- is important.
module Taglib.BinaryOutput where

import Data.Bits
import Data.Word
import Data.Char
import System.IO
import Data.Array.IO
import qualified Data.ByteString as BS

import Taglib.BinaryIOCommon

btoc :: Word8 -> Char
btoc = chr . fromEnum

-- | Puts a word into the specified index of a byte array in
-- little endian byte order.
put16LE :: Buffer -> Int -> Word16 -> IO ()
put16LE buf i w =
    let w32 :: Word32
        w32 = fromIntegral w
        lowByte = w32 .&. firstByteMask
        highByte = shiftR w32 8 .&. firstByteMask
    in do
        writeArray buf i (fromIntegral lowByte)
        writeArray buf (i + 1) (fromIntegral highByte)

-- | Puts a word into the specified index of a byte array in
-- big endian byte order.
put16BE :: Buffer -> Int -> Word16 -> IO ()
put16BE buf i w = do
    put16LE buf i w
    rev2 buf i

-- | Puts a double word into the specified index of a byte array in
-- little endian byte order.
put32LE :: Buffer -> Int -> Word32 -> IO ()
put32LE buf i dw =
    let b1 = dw .&. firstByteMask
        b2 = shiftR dw 8 .&. firstByteMask
        b3 = shiftR dw 16 .&. firstByteMask
        b4 = shiftR dw 24 .&. firstByteMask
    in do
        writeArray buf i (fromIntegral b1)
        writeArray buf (i + 1) (fromIntegral b2)
        writeArray buf (i + 2) (fromIntegral b3)
        writeArray buf (i + 3) (fromIntegral b4)

-- | Puts a double word into the specified index of a byte array in
-- big endian byte order.
put32BE :: Buffer -> Int -> Word32 -> IO ()
put32BE buf i dw = do
    put32LE buf i dw
    rev4 buf i

-- | Writes a word into the specified handle in little endian byte order.
write16LE :: Handle -> Word16 -> IO ()
write16LE handle w = do
    arr <- createBuffer 2
    put16LE arr 0 w
    hPutArray handle arr 2

-- | Writes a word into the specified handle in big endian byte order.
write16BE :: Handle -> Word16 -> IO ()
write16BE handle w = do
    arr <- createBuffer 2
    put16LE arr 0 w
    rev2 arr 0
    hPutArray handle arr 2

-- | Writes a double word into the specified handle in little endian byte order.
write32LE :: Handle -> Word32 -> IO ()
write32LE handle w = do
    arr <- createBuffer 4
    put32LE arr 0 w
    hPutArray handle arr 4

-- | Writes a double word into the specified handle in big endian byte order.
write32BE :: Handle -> Word32 -> IO ()
write32BE handle w = do
    arr <- createBuffer 4
    put32LE arr 0 w
    rev4 arr 0
    hPutArray handle arr 4

-- | Writes the given byte (represented as 'Word8') into the given file handle.
writeWord8 :: Handle -> Word8 -> IO ()
writeWord8 handle b = hPutChar handle (btoc b)

-- | Writes the given list of bytes into the given file handle.
writeData :: Handle -> [Word8] -> IO ()
writeData _ [] = return ()
writeData handle bytes = BS.hPut handle (BS.pack bytes)
