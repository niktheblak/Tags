-- | Implementation of the unsynchronization and synchsafe integer codecs
-- described in ID3v2 specification chapter 6.
-- 
-- A synchsafe integer is one where the most significant bit of each byte is
-- zero. A synchsafe integer can be constructed by shifting the bits left
-- on a per-byte basis. This module has functions to pack and unpack synchsafe
-- integers as well as encode or decode byte arrays according to the
-- unsynchronization scheme.
module Taglib.Id3v2.SynchSafe(maxSynchSafeValue,
                               maxLargeSynchSafeValue,
                               toSynchSafe,
                               fromSynchSafe,
                               toLargeSynchSafe,
                               fromLargeSynchSafe,
                               encodeUnsync,
                               decodeUnsync) where

import Data.Bits
import Data.Word

-- | Bit pattern 00000000 00000000 00000000 11111111
byte1 :: Word32
byte1 = 0xFF

ssmask0, ssmask1, ssmask2, ssmask3 :: Word32
ssmask0 = 0xFF
ssmask1 = (ssmask0 `shiftL` 8) .|. (1 `shiftL` 8)
ssmask2 = (ssmask0 `shiftL` 16) .|. (3 `shiftL` 16)
ssmask3 = (ssmask0 `shiftL` 24) .|. (7 `shiftL` 24)

ssmask4 :: Integer
ssmask4 = (fromIntegral ssmask0 `shiftL` 32) .|. (15 `shiftL` 32)

-- | The maximum value of a 28-bit synchsafe integer.
maxSynchSafeValue :: Word32
maxSynchSafeValue = 0xFFFFFFF

-- | The maximum value of a 35-bit synchsafe integer.
maxLargeSynchSafeValue :: Integer
maxLargeSynchSafeValue = 0x7FFFFFFFF

-- | Retrieves the n least significant bits of an integer.
--
-- This function is equivalent to doing an AND operation of the integer
-- and a word with n least significant bytes set to one.
nlast :: (Bits a, Num a) => a -- ^ An integer whose bits can be addressed.
    -> Int -- ^ Specifies how many least significant bits to include.
    -> a -- ^ The n least significant bits of the integer.
nlast w n = w .&. (allOnes `shiftR` c)
    where
        allOnes = complement 0
        c = bitSize w - n

bytes32LE :: Word32 -> [Word8]
bytes32LE w = [b0, b1, b2, b3]
    where
        b0 = fromIntegral b0'
        b1 = fromIntegral b1'
        b2 = fromIntegral b2'
        b3 = fromIntegral b3'
        b0' = w .&. byte1
        b1' = (w `shiftR` 8) .&. byte1
        b2' = (w `shiftR` 16) .&. byte1
        b3' = w `shiftR` 24

toWord32LE :: [Word8] -> Word32
toWord32LE (b0 : b1 : b2 : b3 : bs) =
    b0' .|. (b1' `shiftL` 8) .|. (b2' `shiftL` 16) .|. (b3' `shiftL` 24)
    where
        b0' = fromIntegral b0
        b1' = fromIntegral b1
        b2' = fromIntegral b2
        b3' = fromIntegral b3
toWord32LE _ = error "The argument Word8 list must have at least 4 members"

toWord40LE :: [Word8] -> Integer
toWord40LE (b0 : b1 : b2 : b3 : b4 : bs) =
    b0' .|. (b1' `shiftL` 8) .|. (b2' `shiftL` 16) .|. (b3' `shiftL` 24) .|. (b4' `shiftL` 32)
    where
        b0' = fromIntegral b0
        b1' = fromIntegral b1
        b2' = fromIntegral b2
        b3' = fromIntegral b3
        b4' = fromIntegral b4
toWord40LE _ = error "The argument Word8 list must have at least 5 members"

bytes40LE :: Integer -> [Word8]
bytes40LE w = [ b0, b1, b2, b3, b4 ]
    where
        b0 = fromIntegral b0'
        b1 = fromIntegral b1'
        b2 = fromIntegral b2'
        b3 = fromIntegral b3'
        b4 = fromIntegral b4'
        b0' = w .&. byte1'
        b1' = (w `shiftR` 8) .&. byte1'
        b2' = (w `shiftR` 16) .&. byte1'
        b3' = (w `shiftR` 24) .&. byte1'
        b4' = (w `shiftR` 32) .&. byte1'
        byte1' = fromIntegral byte1

-- | Encodes an unsigned integer to 28-bit synchsafe form.
toSynchSafe :: Word32 -> Word32
toSynchSafe n
    | n > maxSynchSafeValue = error "Value over maximum synchsafe range."
    | otherwise = p0 .|. p1 .|. p2 .|. p3
        where
            -- Take the shifted bytes and add leftover bits from the previous
            -- bytes
            p0 = clearBit p0' 7
            p1 = clearBit (p1' .|. prev0) 15
            p2 = clearBit (p2' .|. prev1) 23
            p3 = clearBit (p3' .|. prev2) 31
            -- Calculate leftovers from previous bytes
            prev0 = shiftL (if testBit n 7 then 1 else 0) 8
            prev1 = shiftL (if testBit n 15 then 1 else 0) 17
            prev2 = shiftL (if testBit n 23 then 1 else 0) 26
            -- Bytes 0, 1, 2 and 3 shifted and most significant bit zeroed
            p0' = n .&. ssmask0
            p1' = (n `shiftL` 1) .&. ssmask1
            p2' = (n `shiftL` 2) .&. ssmask2
            p3' = (n `shiftL` 3) .&. ssmask3

-- | Decodes an unsigned integer from 28-bit synchsafe form.
fromSynchSafe :: Word32 -> Word32
fromSynchSafe w = toWord32LE [p0, p1, p2, p3]
    where
        -- Take the rightshifted bytes and append the last bits from the
        -- previous bytes
        p0 = b0 .|. (last1 `shiftL` 7)
        p1 = b1' .|. (last2 `shiftL` 6)
        p2 = b2' .|. (last3 `shiftL` 5)
        p3 = b3 `shiftR` 3
        -- Last bits from the previous byte
        last1 = nlast b1 1
        last2 = nlast b2 2
        last3 = nlast b3 3
        -- Rightshift bytes 1 and 2
        b1' = (b1 `shiftR` 1)
        b2' = (b2 `shiftR` 2)
        -- The bytes of the dword
        [b0, b1, b2, b3] = bytes32LE w

-- | Encodes an unsigned integer to 35-bit synchsafe form.
toLargeSynchSafe :: Integer -> [Word8]
toLargeSynchSafe n
    | n > maxLargeSynchSafeValue = error "Value over maximum large synchsafe range."
    | n < 0 = error "Synchsafe integer cannot be negative."
    | otherwise = bytes40LE (p0 .|. p1 .|. p2 .|. p3 .|. p4)
        where
            -- Take the shifted bytes and add leftover bits from the previous
            -- bytes
            p0 = clearBit p0' 7
            p1 = clearBit (p1' .|. prev0) 15
            p2 = clearBit (p2' .|. prev1) 23
            p3 = clearBit (p3' .|. prev2) 31
            p4 = clearBit (p4' .|. prev3) 39
            -- Calculate leftovers from previous bytes
            prev0 = shiftL (if testBit n 7 then 1 else 0) 8
            prev1 = shiftL (if testBit n 15 then 1 else 0) 17
            prev2 = shiftL (if testBit n 23 then 1 else 0) 26
            prev3 = shiftL (if testBit n 31 then 1 else 0) 35
            -- Bytes 0, 1, 2 and 3 shifted and most significant bit zeroed
            p0' = n .&. ssmask0'
            p1' = (n `shiftL` 1) .&. ssmask1'
            p2' = (n `shiftL` 2) .&. ssmask2'
            p3' = (n `shiftL` 3) .&. ssmask3'
            p4' = (n `shiftL` 4) .&. ssmask4
            ssmask0' = fromIntegral ssmask0
            ssmask1' = fromIntegral ssmask1
            ssmask2' = fromIntegral ssmask2
            ssmask3' = fromIntegral ssmask3

-- | Decodes an unsigned integer from 28-bit synchsafe form.
fromLargeSynchSafe :: [Word8] -> Integer
fromLargeSynchSafe (b0 : b1 : b2 : b3 : b4 : bs) =
    toWord40LE [p0, p1, p2, p3, p4]
    where
        -- Take the rightshifted bytes and append the last bits from the
        -- previous bytes
        p0 = b0 .|. (last1 `shiftL` 7)
        p1 = b1' .|. (last2 `shiftL` 6)
        p2 = b2' .|. (last3 `shiftL` 5)
        p3 = b3' .|. (last4 `shiftL` 4)
        p4 = b4 `shiftR` 4
        -- Last bits from the previous byte
        last1 = nlast b1 1
        last2 = nlast b2 2
        last3 = nlast b3 3
        last4 = nlast b4 4
        -- Rightshift bytes 1, 2 and 3
        b1' = (b1 `shiftR` 1)
        b2' = (b2 `shiftR` 2)
        b3' = (b3 `shiftR` 3)
fromLargeSynchSafe _ = error "The argument Word8 list must have at least 5 members"

encodeUnsync :: [Word8] -> [Word8]
encodeUnsync = concatMap (\b -> if b == 0xFF then [b, 0] else [b])
{-encodeUnsync (b : bs)
    | b == 0xFF = b : 0 : encodeUnsync bs
    | otherwise = b : encodeUnsync bs
encodeUnsync [] = []-}

decodeUnsync :: [Word8] -> [Word8]
decodeUnsync (b0 : b1 : bs)
    | b0 == 0xFF = b0 : decodeUnsync bs
    | otherwise = b0 : decodeUnsync (b1 : bs)
decodeUnsync (b : []) = [b]
decodeUnsync [] = []

