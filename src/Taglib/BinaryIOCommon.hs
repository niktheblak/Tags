-- | Common definitions for modules 'BinaryInput' and 'BinaryOutput'.
module Taglib.BinaryIOCommon where

import Data.Word
import Data.Array.IO

-- | Defines a type for byte buffer.
type Buffer = IOUArray Int Word8

-- | Creates a 'Buffer' instance with the specified length.
createBuffer :: Int -> IO Buffer
createBuffer len = newArray_ (0, len)

-- | A byte mask containing eight ones.
firstByteMask :: Word32
firstByteMask = 0xFF

-- | Reverses the order of 4 bytes at the specified position of a buffer.
rev4 :: Buffer -> Int -> IO ()
rev4 buf i = rev' buf i (i + 3)

-- | Reverses the order of 2 bytes at the specified position of a buffer.
rev2 :: Buffer -> Int -> IO ()
rev2 buf i = do
    b1 <- readArray buf i
    b2 <- readArray buf (i + 1)
    writeArray buf i b2
    writeArray buf (i + 1) b1

-- | Reverses the order of given number of bytes from a given position of
-- a buffer.
rev' :: Buffer -- ^ The buffer.
    -> Int -- ^ Start index within the buffer.
    -> Int -- ^ End index within the buffer.
    -> IO ()

rev' buf start end
    | start < end = do
        b <- readArray buf start
        writeArray buf end b
        rev' buf (start + 1) (end - 1)
    | otherwise = do return ()

-- | Concatenates two buffers into a new buffer.
catBuffers :: Buffer -> Buffer -> IO Buffer
catBuffers buf1 buf2 =
    let copy :: Buffer -> Int -> Buffer -> Int -> IO ()
        copy target targetIndex source i = do
            tb <- getBounds target
            let tlen = snd tb
            if i < tlen then do
                b <- readArray source i
                writeArray target (i + targetIndex) b
                copy target targetIndex source (i + 1)
                else return ()
    in do
        buf1b <- getBounds buf1
        buf2b <- getBounds buf2
        let buf1Len = snd buf1b
        let buf2Len = snd buf2b
        let resLen = buf1Len + buf2Len
        resBuf <- createBuffer resLen
        copy resBuf 0 buf1 0
        copy resBuf buf1Len buf2 0
        return resBuf

-- | Blits a list of bytes onto a buffer starting from a specified position.
blitList :: Buffer -- ^ The buffer to copy the byte list to.
    -> Int -- ^ Starting position within the buffer.
    -> [Word8] -- ^ List of bytes to be copied into the buffer.
    -> IO ()

blitList buf idx arr =
    let worker i =
            if i < length arr
                then do
                    writeArray buf (i + idx) (arr !! i)
                    worker (i + 1)
                else return ()
    in do
        arrb <- getBounds buf
        let arrLen = snd arrb
        if idx + length arr > arrLen
            then error ""
            else do
                worker 0
                return ()
