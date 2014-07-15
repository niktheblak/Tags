-- | Module for conversion between Haskell strings and ASCII.
module Tags.Ascii where

import Data.Char
import Data.Word

-- | Determines whether the given string is ASCII compatible, i.e. contains
-- only ASCII characters.
isAsciiString :: String -> Bool
isAsciiString = all isAscii

-- | Determines whether the given byte array is ASCII compatible, i.e. contains
-- only bytes within ASCII range.
isAsciiBytes :: [Word8] -> Bool
isAsciiBytes bs = all isAscii [(chr . fromEnum) b | b <- bs]

-- | Converts a Haskell string into ASCII. The string must contain only ASCII
-- characters.
toAscii :: String -> [Word8]
toAscii str =
    [ if isAscii c
        then toEnum (ord c)
        else error "Character value does not fit into ASCII range."
        | c <- str ]

-- | Converts ASCII data into a Haskell string.
fromAscii :: [Word8] -> String
fromAscii ws = [chr (fromEnum c) | c <- ws]

-- | Converts a Haskell string into ASCII replacing non-ASCII charactes with
-- a question mark.
toAsciiR :: String -> [Word8]
toAsciiR = map (replaceIfNot isAscii)

-- | Converts a Haskell string into ASCII replacing non-ASCII charactes with
-- user-specified replacement character.
toAsciiR' :: String -> Char -> [Word8]
toAsciiR' str r =
    map (replaceIfNot' isAscii r) str

-- | Converts a Haskell string into ISO-8859-1 (Latin1). The string must
-- contain only Latin1 characters (Unicode code points 0-255).
toLatin1 :: String -> [Word8]
toLatin1 str =
    [ if isLatin1 c
        then toEnum (ord c)
        else error "Character value does not fit into ASCII range."
        | c <- str ]

-- | Converts a Haskell string into Latin1 replacing non-ASCII charactes with
-- a question mark.
toLatin1R :: String -> [Word8]
toLatin1R = map (replaceIfNot isLatin1)

fromLatin1 :: [Word8] -> String
fromLatin1 bs = [(chr . fromEnum) b | b <- bs]

replaceIfNot :: (Char -> Bool) -> Char -> Word8
replaceIfNot f c =
    if f c then toEnum (ord c) else toEnum (ord '?')

replaceIfNot' :: (Char -> Bool) -> Char -> Char -> Word8
replaceIfNot' f r c =
    if f c then toEnum (ord c) else toEnum (ord r)
