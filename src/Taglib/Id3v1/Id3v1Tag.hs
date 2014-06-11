-- | A 'TagItem' oriented implementation of the ID3v1 tag specification.
--
-- The ID3v1 is a very limited format by nature. The ID3v1 tag consists of
-- a fixed 128 byte block divided into couple of fixed-length fields with
-- fixed definitions.
-- The detailed structure of ID3v1 tag is:
--
-- [@ID3v1 Preamble@] 3 bytes (ASCII text string @TAG@)
--
-- [@Title@] 30 bytes (maximum of 30 ASCII characters, unused characters have zero value)
--
-- [@Artist@] 30 bytes (maximum of 30 ASCII characters, unused characters have zero value)
--
-- [@Album@] 30 bytes (maximum of 30 ASCII characters, unused characters have zero value)
--
-- [@Year@] 4 bytes (maximum of 4 ASCII characters, usually digits)
--
-- [@Comment@] 30 bytes (maximum of 30 ASCII characters, unused characters have zero value)
--
-- [@Genre identifier@] 1 byte
--    
-- Since locale information is not included in the tag,
-- only ASCII characters can be reliably used for ID3v1 field values.
-- Usage of extended codepages, such as Latin1, is possible but not recommended
-- since the reader has no knowledge of the used encoding.
-- UTF-8 encoding is possible but not
-- recommended because of lacking software support and the hard limit of 30
-- bytes for text fields. This hard limit may cut off a variable-length
-- encoded UTF-8 string in the middle of a surrogate, causing malformed
-- characters.
--
-- Whenever an ID3v1 tag is written, the 'TagItem' data
-- is truncated to the maximum width of the field.
-- The truncation is done during the serialization phase so the written
-- data might differ from the data in the 'TagItem' instance.
--
-- The ID3 v1.1 specification adds a track number field to ID3 v1.0 tag.
-- If one of the 'TagItem's given to 'processItems' function has a key
-- @TRACKNUMBER@, the tag is treated as ID3 v1.1. Otherwise the tag
-- is treated as ID3 v1.0.
--
-- There is no \"genre is not set\" identifier for music genre.
-- Usually genre number 0 (Blues) is used to mean that the genre is not set,
-- so the genre identifier for blues is basically not usable.
-- This is another fine example of the sheer brilliance of ID3v1 design.
--
-- This implementation supports the 'TagItem' interface whenever
-- appropriate. 'TagItem's with unsupported keys will be dropped from
-- the tag during serialization.
--
-- Fore more information, see the ID3v1 tag specification <http://www.id3.org>
module Taglib.Id3v1.Id3v1Tag(id3v1Length,
                             titleLength,
                             artistLength,
                             albumLength,
                             yearLength,
                             commentLength,
                             acceptedKeys,
                             processItems,
                             createImage,
                             writeId3v1Tag,
                             readId3v1Tag) where

import Data.Char
import Data.Word
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Map as Map
import System.IO
import System.Log.Logger

import Taglib.ItemData
import Taglib.Signatures
import Taglib.StringUtils
import Taglib.Id3v1.Genres
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- | Length of a complete ID3v1 tag in bytes.
id3v1Length :: Int
id3v1Length = 128

-- | Length of the TITLE field.
titleLength :: Int
titleLength = 30

-- | Length of the ARTIST field.
artistLength :: Int
artistLength = 30

-- | Length of the ALBUM field.
albumLength :: Int
albumLength = 30

-- | Length of the YEAR field.
yearLength :: Int
yearLength = 4

-- | Length of the COMMENT field.
commentLength :: Int
commentLength = 30

titleOffset, artistOffset, albumOffset, yearOffset, commentOffset, genreOffset, trackNumberOffset :: Int

titleOffset = 3
artistOffset = titleOffset + titleLength
albumOffset = artistOffset + artistLength
yearOffset = albumOffset + albumLength
commentOffset = yearOffset + yearLength
genreOffset = id3v1Length - 1
trackNumberOffset = commentOffset + commentLength - 1

-- | Key for the @TITLE@ field of ID3v1 tag format.
titleKey :: String
titleKey = "TITLE"

-- | Key for the @ARTIST@ field of ID3v1 tag format.
artistKey :: String
artistKey = "ARTIST"

-- | Key for the @ALBUM@ field of ID3v1 tag format.
albumKey :: String
albumKey = "ALBUM"

-- | Key for the @YEAR@ field of ID3v1 tag format.
--
-- The @YEAR@ field (sometimes called the @DATE@ field) in ID3v1 contains
-- a four-digit year number such as @2005@. Actually the field contains four
-- ASCII characters, which are most commonly digits but other interpretations
-- do exist. The four-digit year number format is recommended.
yearKey :: String
yearKey = "YEAR"

-- | Key for the @COMMENT@ field of ID3v1 tag format.
commentKey :: String
commentKey = "COMMENT"

-- | Key for the @TRACKNUMBER@ (also known as @TRACK@) field of ID3v1 tag
-- format.
--
-- The track number is internally stored as one byte, thus allowing a track
-- number range of 0-255. This library tries to convert the string content of a
-- 'TagItem' instance with this key to a 1-byte integer before writing the tag.
trackNumberKey :: String
trackNumberKey = "TRACKNUMBER"

-- | Key for the @GENRE@ field of ID3v1 tag format.
--
-- The genre field is internally stored as one byte genre identifier.
-- The ID3v1 specification contains a list of 127 musical genre names.
-- These were later extended to 255 names by Winamp.
-- The Winamp extension is the
-- current de facto standard for the ID3v1 genre names and this extension is
-- used by this library.
--
-- This library first tries to map the given genre name into the
-- corresponding genre identifier number from the list of
-- Winamp ID3v1 genres. If that fails, genre number 0 (Blues) will be used.
--
-- There is no \"genre is not set\" identifier for music genre.
-- Usually genre number 0 (Blues) is used to mean that the genre is not set,
-- so the genre identifier for blues is basically not usable.
-- This is another fine example of the sheer brilliance of ID3v1 design.
genreKey :: String
genreKey = "GENRE"

-- | A list of keys supported by the ID3v1 specification.
acceptedKeys :: [String]
acceptedKeys = [ titleKey, artistKey, albumKey, yearKey, commentKey, trackNumberKey, genreKey ]

titleIds, artistIds, albumIds, yearIds, commentIds, genreIds, trackNumberIds :: [String]

titleIds = [ "TITLE", "TIT2" ]
artistIds = [ "ARTIST", "PERFORMER", "CONDUCTOR", "TPE1", "TPE2", "TPE3" ]
albumIds = [ "ALBUM", "TALB" ]
yearIds = [ "YEAR", "DATE" ]
commentIds = [ "COMMENT", "COMM" ]
genreIds = [ "GENRE", "STYLE" ]
trackNumberIds = [ "TRACKNUMBER", "TRACK" ]

-- | Processes the given list of 'TagItem' instances for ID3v1 serialization.
--
-- This function:
--
-- (1) Converts 'TagItem' instances to textual key-value pairs
--
-- (2) Maps alternate names (such as @PERFORMER@, @DATE@, @STYLE@) for ID3v1
-- keys to canonical ID3v1 keys
processItems :: (TagItem a) => [a] -> [(String, String)]
processItems items =
    [(convertKey (ukey i), show (value i)) | i <- items]
    where
        ukey = toUpperCase . key
        convertKey k
            | k `elem` titleIds = titleKey
            | k `elem` artistIds = artistKey
            | k `elem` albumIds = albumKey
            | k `elem` yearIds = yearKey
            | k `elem` commentIds = commentKey
            | k `elem` genreIds = genreKey
            | k `elem` trackNumberIds = trackNumberKey
            | otherwise = k

-- | Converts a list of 'TagItem' instances into a map containing
-- the 'TagItem' keys as keys and 'TagItem' values as values.
toMap :: (TagItem a) => [a] -> Map.Map String String
toMap items = Map.fromList ipairs
    where ipairs = filterKeyValuePairs (processItems items)    
        
-- | Generates a 128-byte binary image of an ID3v1 tag from the given
-- list of tag items.
createImage :: (TagItem a) => [a] -> BS.ByteString
createImage = generateTagBS . toMap

-- | Generates a 128-byte binary image of an ID3v1 tag from the given
-- map containing tag keys and values.
generateTagBS :: Map.Map String String -> BS.ByteString
generateTagBS mp =
    BS.concat [sig, title, artist, album, year, comment, genre]
    where
        sig = BS.pack sigId3v1
        hasTrackNumber = Map.member trackNumberKey mp
        hasGenre = Map.member genreKey mp
        commentStr = Map.findWithDefault "" commentKey mp
        title = toArr (Map.findWithDefault "" titleKey mp) titleLength
        artist = toArr (Map.findWithDefault "" artistKey mp) artistLength
        album = toArr (Map.findWithDefault "" albumKey mp) albumLength
        year = toArr (Map.findWithDefault "" yearKey mp) yearLength
        comment = if hasTrackNumber then
            let arr = toArr commentStr (commentLength - 2)
                n :: Int
                n = read (mp Map.! trackNumberKey)
                term = BSC.pack ('0' : [chr n])
            in arr `BS.append` term
            else toArr commentStr commentLength
        genre = BS.pack (if hasGenre then
            let genreCode = getGenreCode (mp Map.! genreKey) in
            case genreCode of
                Just c -> [toEnum c]
                Nothing -> [0]
            else [0])
        toArr :: String -> Int -> BS.ByteString
        toArr [] len = truncBS BS.empty len
        toArr s len = truncBS (BS.pack (UTF8.encode s)) len

truncBS :: BS.ByteString -> Int -> BS.ByteString
truncBS val len =
    if len > BS.length val then
        val `BS.append` getZeroes (len - BS.length val)
    else BS.take len val
    
toTwoDigit :: Int -> String
toTwoDigit n
    | n < 10 = show n
    | otherwise = '0' : show n

getZeroes :: Int -> BS.ByteString
getZeroes n = BS.pack (take n (repeat 0))

-- | Removes items with unsupported keys from a list of 'TagItem's.
filterTagItems :: (TagItem a) => [a] -> [a]
filterTagItems =
    filter (\i -> elem (toUpperCase (key i)) acceptedKeys)

-- | Removes key-value pairs with unsupported keys from a list of
-- key-value pairs.
filterKeyValuePairs :: [(String, String)] -> [(String, String)]
filterKeyValuePairs =
    filter (\i -> elem (toUpperCase (fst i)) acceptedKeys)

-- | Writes the given list of 'TagItem's into the specified handle as
-- ID3v1 tag.
--
-- Some tag items may be truncated or dropped completely if they have
-- an unsupported key.
writeId3v1Tag :: (TagItem a) => Handle -> [a] -> IO ()
writeId3v1Tag handle items =
    writeId3v1Tag' handle itemTbl
    where itemTbl = toMap items
    

writeId3v1Tag' :: Handle -> Map.Map String String -> IO ()
writeId3v1Tag' handle mp =
    let tagData = generateTagBS mp in
    do
        infoM "Id3v1Tag.writeId3v1Tag'" "Writing ID3v1 tag..."
        BS.hPut handle tagData

stringify :: [Word8] -> String
stringify arr = [chr (fromEnum b) | b <- arr, b /= 0]

-- | Drops pairs with zero-length value from a given list.
dropEmpties :: [(String, String)] -> [(String, String)]
dropEmpties = filter (\(_, v) -> length v > 0)

-- | Gets the track number from a given ByteString image of an ID3v1 tag,
-- if it is set.
getTrackNumberBS :: BS.ByteString -> Maybe Int
getTrackNumberBS id3data =
    if b1 == 0 && b2 /= 0 then Just (fromEnum b2) else Nothing
    where
        b1 = id3data `BS.index` trackNumberOffset - 1
        b2 = id3data `BS.index` trackNumberOffset

-- | Gets the track number from a given image of an ID3v1 tag, if it is set.
getTrackNumber :: [Word8] -> Maybe Int
getTrackNumber id3data =
    if b1 == 0 && b2 /= 0 then Just (fromEnum b2) else Nothing
    where
        b1 = id3data !! trackNumberOffset - 1
        b2 = id3data !! trackNumberOffset

-- | Reads an ID3v1 tag from a specified handle.
readId3v1Tag :: Handle -> IO [(String, String)]
readId3v1Tag handle =
    let dropZeroes str = stringify (BS.unpack str) in
    do
        id3data <- BS.hGet handle id3v1Length
        let preamb = BS.take (length sigId3v1) id3data
        if preamb /= BS.pack sigId3v1
            then do
                errorM "Id3v1Tag.readId3v1Tag" ("Invalid ID3v1 preamble: " ++ (getPrintables $ BSC.unpack preamb))
                ioError (userError "Invalid ID3v1 preamble.")
            else return ()
        let title = dropZeroes (BS.take titleLength (BS.drop titleOffset id3data))
            artist = dropZeroes (BS.take artistLength (BS.drop artistOffset id3data))
            album = dropZeroes (BS.take albumLength (BS.drop albumOffset id3data))
            year = dropZeroes (BS.take yearLength (BS.drop yearOffset id3data))
            comment = dropZeroes (BS.take commentLength (BS.drop commentOffset id3data))
            genreCode = fromEnum (id3data `BS.index` genreOffset)
            genre = getGenreName genreCode
            items = [(titleKey, title),
                    (artistKey, artist),
                    (albumKey, album),
                    (yearKey, year)]
            -- We make a concious decision to drop genre code 0 (Blues) since
            -- it usually indicates that the genre is not set. Sorry for Blues
            -- fans though.
            listWithGenre = if genreCode /= 0
                then (genreKey, genre) : items
                else items
            finalList = case getTrackNumberBS id3data of
                Just n -> (trackNumberKey, toTwoDigit n) : listWithGenre
                Nothing -> listWithGenre
            in return (dropEmpties finalList)