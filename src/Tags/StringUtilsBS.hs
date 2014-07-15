-- | Similar to 'StringUtils' module but for the 'ByteString' type.
module Tags.StringUtilsBS where

import Data.Word
import Data.Char
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Tags.StringUtils(isPrintable)
    
replacement :: Char
replacement = '.'

-- | Removes whitespace from the beginning and the end of a string.
trim :: BC.ByteString -> BC.ByteString
trim = trimEnd . trimStart
    where
        trimStart = BC.dropWhile isSpace
        trimEnd = BC.reverse . trimStart . BC.reverse

getPrintables :: BC.ByteString -> BC.ByteString
getPrintables str =
    BC.map getPrn str
    where
        getPrn :: Char -> Char
        getPrn c = if isPrint c then c else replacement

bytesToPrintableString :: B.ByteString -> B.ByteString
bytesToPrintableString arr =
    B.map getPrn arr
    where
        getPrn :: Word8 -> Word8
        getPrn b = if isPrintable b
            then b
            else toEnum (ord replacement)

getAlphas :: BC.ByteString -> BC.ByteString
getAlphas = BC.filter isAlpha

normalizedForm :: BC.ByteString -> BC.ByteString
normalizedForm =
    BC.map toUpper . getAlphas

toUpperCase :: BC.ByteString -> BC.ByteString
toUpperCase = BC.map toUpper

-- | Replaces a part of a given 'ByteString' with the contents of another
-- 'ByteString'.
blitB :: B.ByteString -- ^ The string whose contents are to be replaced.
    -> Int -- ^ The index in the string after which the contents of the other
           -- are copied.
    -> B.ByteString -- ^ Contents of this string are copied into the first
                     -- string. This string must be shorter than the first string.
    -> B.ByteString -- ^ The first string with some contents replaced by the
                     -- second string.
blitB target idx source =
    let start = B.take idx target
        middle = source
        end = B.drop (idx + B.length source) target
    in B.concat [start, middle, end]

repeatB :: Int -> Char -> BC.ByteString
repeatB n c = BC.pack (take n (repeat c))
