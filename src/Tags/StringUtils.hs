-- | Utilities for dealing with text.
module Tags.StringUtils where

import Data.Bits
import Data.Word
import Data.Char
import Data.List(elemIndex)

-- | The replacement character that is used by the 'getPrintables' and
-- 'bytesToPrintableString' functions.
replacement :: Char
replacement = '.'

-- | Removes whitespace from the beginning and the end of a string.
trim :: String -> String
trim = trimEnd . trimStart
    where
        trimStart = dropWhile isSpace
        trimEnd = reverse . trimStart . reverse

-- | Removes all whitespace from a string.
removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

-- | Gets the printable characters of a string.
getPrintables :: String -> String
getPrintables str =
    map getPrn str
    where
        getPrn :: Char -> Char
        getPrn c = if isPrint c then c else replacement

-- | Gets the printable characters of a byte array.
bytesToPrintableString :: [Word8] -> String
bytesToPrintableString arr =
    map getPrn arr
    where
        getPrn :: Word8 -> Char
        getPrn b = if isPrintable b then chr (fromEnum b) else replacement

-- | Determines whether a byte value is the value of a printable Unicode
-- character.
isPrintable :: Word8 -> Bool
isPrintable n =
    isPrint (chr c)
    where c = fromEnum n

-- | Gets only the alphabetic characters (a-z, A-Z) of the given string.
getAlphas :: String -> String
getAlphas = filter isAlpha

-- | Gets the alphabetical characters (a-z, A-Z) of the given string converted
-- to upper case.
--
-- This form (uppercase, all-alphabetic) is called normalized, or canonical,
-- form. It is useful in performing text searches that ignore case and
-- punctuation.
normalizedForm :: String -> String
normalizedForm =
    map toUpper . getAlphas

-- | Converts a given string to upper case.
toUpperCase :: String -> String
toUpperCase = map toUpper

-- | Converts a given string to lower case.
toLowerCase :: String -> String
toLowerCase = map toLower

-- | Capitalizes a given string, i.e. converts the first letter into upper case
-- and the remaining letters to lower case.
capitalize :: String -> String
capitalize [] = []
capitalize (c : cs) = toUpper c : toLowerCase cs

-- | Splits a string at a specified character.
splitAtChar :: Char -> String -> Maybe (String, String)
splitAtChar c str = do
    idx <- elemIndex c str
    let left = take idx str
    let right = drop (idx + 1) str
    return (left, right)

-- | Inserts a separator string into a string after every character from which
-- the function @f@ returns @True@.
separateBy :: (Char -> Bool) -- ^ Function @f@ that determines whether to insert
                             -- a separator after a character.
    -> String -- ^ Separator string.
    -> String -- ^ String to process.
    -> String -- ^ Processed string.

separateBy _ _ [] = []
separateBy f separator str =
    head str : concat tokens
    where
        tokens = map (\c -> if f c then separator ++ [c] else [c]) (tail str)

-- | Inserts a space after each capital letter in the string.
--
-- For example, "separateByCap" becomes "separate By Cap".
separateByCap :: String -> String
separateByCap = separateBy isUpper " "

-- | Finds the index of a string in a list of strings ignoring case.
--
-- This function is similar to 'elemIndex' for String arguments with
-- ignored case.
caseInsensitiveFind :: String -> [String] -> Maybe Int
caseInsensitiveFind str strings =
    elemIndex (toUpperCase str) (map toUpperCase strings)

-- | Finds the index of a string in a list of strings ignoring case and
-- punctuation.
--
-- This function is similar to 'elemIndex' for String arguments with
-- ignored case and non-alphabetic characters filtered out.
normalizedFormFind :: String -> [String] -> Maybe Int
normalizedFormFind str strings =
    elemIndex (normalizedForm str) (map normalizedForm strings)

-- | Determines whether a given list is a sublist of another list.
indexOf :: Eq a => [a] -- ^ Pattern that is searched from the other list.
    -> [a] -- ^ List the pattern is searched from.
    -> Maybe Int -- ^ If the specified pattern appears as a sublist of the
                 -- list, a 'Just' containing the starting index of the sublist.
                 -- Otherwise 'Nothing'.

indexOf [] _ = Nothing
indexOf pat str =
    let patLen = length pat
        worker str' index
            | length str' < patLen = Nothing
            | otherwise =
                case elemIndex (head pat) str' of
                    Just i ->
                        if i + patLen > length str' then Nothing else
                            let substr = take patLen (drop i str') in
                            if substr == pat then Just (index + i)
                                else worker (tail str') (index + 1)
                    Nothing -> Nothing
    in
    worker str 0

-- | Shows an integer in binary representation.
showBinary :: (Bits a) => a -> String
showBinary n = reverse b
    where
        b = map bitToStr [0 .. bitSize n - 1]
        bitToStr i = if testBit n i then '1' else '0'

-- | Inserts a separator character to every nth position in a string.
--
-- An example: group binary digits of a number by eight:
--
-- > groupBy 8 ' ' (showBinary 99)
--
groupBy :: Int -- ^ Specifies how many characters there are between separators.
    -> Char -- ^ The separator character.
    -> String -- ^ The string.
    -> String -- ^ The string with separator characters added every n characters.
groupBy n s str
    | n < 1 = error "Grouping specifier must be at least 1."
    | otherwise = concatMap insert (zip str [1..])
        where
            insert (c, i) = if i `mod` n == 0 then c : [s] else [c]
        
