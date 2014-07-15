-- | Common definitions for modules 'BinaryInput' and 'BinaryOutput'.
module Tags.BinaryIOCommon where

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

