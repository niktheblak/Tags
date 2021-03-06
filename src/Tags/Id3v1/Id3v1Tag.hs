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
--
-- This implementation supports the 'TagItem' interface whenever
-- appropriate. 'TagItem's with unsupported keys will be dropped from
-- the tag during serialization.
--
-- Fore more information, see the ID3v1 tag specification <http://www.id3.org>
module Tags.Id3v1.Id3v1Tag(id3v1Length,
                           titleLength,
                           artistLength,
                           albumLength,
                           yearLength,
                           commentLength,
                           acceptedKeys,
                           convertToId3v1Items,
                           getId3v1TagData,
                           writeId3v1Tag,
                           readId3v1Tag,
                           readId3v1Tag') where

import Control.Exception(throw, throwIO)
import Control.Monad(when)
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import qualified Data.Encoding as Enc
import Data.Encoding.ISO88591
import qualified Data.Map as Map
import Data.Word
import System.IO
import System.Log.Logger

import Tags.Exceptions
import Tags.ItemData
import Tags.StringUtils
import Tags.Id3v1.Genres

sigId3v1 :: BS.ByteString
sigId3v1 = BSC.pack "TAG"

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
convertToId3v1Items :: (TagItem a) => [a] -> [(String, String)]
convertToId3v1Items items =
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

-- | Generates a 128-byte binary image of an ID3v1 tag from the given
-- list of tag items.
getId3v1TagData :: (TagItem a) => [a] -> BS.ByteString
getId3v1TagData items = getId3v1TagData' mp
    where
        mp = Map.fromList id3v1Items
        id3v1Items = removeInvalidItems (convertToId3v1Items items)
        
getId3v1TagData' :: Map.Map String String -> BS.ByteString
getId3v1TagData' mp =
    BS.concat [sigId3v1, title, artist, album, year, comment, genre]
    where
        hasTrackNumber = Map.member trackNumberKey mp
        hasGenre = Map.member genreKey mp
        commentStr = Map.findWithDefault "" commentKey mp
        title = encodeAndPad (Map.findWithDefault "" titleKey mp) titleLength
        artist = encodeAndPad (Map.findWithDefault "" artistKey mp) artistLength
        album = encodeAndPad (Map.findWithDefault "" albumKey mp) albumLength
        year = encodeAndPad (Map.findWithDefault "" yearKey mp) yearLength
        comment = if hasTrackNumber
            then let trackNumber = read (mp Map.! trackNumberKey)
                in addTrackNumberToComment commentStr trackNumber
            else encodeAndPad commentStr commentLength
        genre = BS.pack (if hasGenre then
            let genreCode = getGenreCode (mp Map.! genreKey) in
            case genreCode of
                Just c -> [toEnum c]
                Nothing -> [0]
            else [0])

addTrackNumberToComment :: String -> Int -> BS.ByteString
addTrackNumberToComment comment trackNumber =
    let arr = encodeAndPad comment (commentLength - 2)
        term = BSC.pack ('\0' : [chr trackNumber])
    in arr `BS.append` term

encodeAndPad :: String -> Int -> BS.ByteString
encodeAndPad [] len = padWithZeroes BS.empty len
encodeAndPad s len = padWithZeroes (Enc.encodeStrictByteString ISO88591 s) len

padWithZeroes :: BS.ByteString -> Int -> BS.ByteString
padWithZeroes val len
    | BS.length val < len =
        let amount = len - BS.length val in
        val `BS.append` BS.replicate amount 0
    | otherwise = BS.take len val
    
toTwoDigit :: Int -> String
toTwoDigit n
    | n < 0 = error "Only non-negative integers are allowed"
    | n >= 10 = show n
    | otherwise = '0' : show n

-- | Removes items with unsupported keys from a list of 'TagItem's.
filterTagItems :: (TagItem a) => [a] -> [a]
filterTagItems =
    filter (\i -> toUpperCase (key i) `elem` acceptedKeys)

-- | Removes key-value pairs with unsupported keys from a list of
-- key-value pairs.
removeInvalidItems :: [(String, String)] -> [(String, String)]
removeInvalidItems =
    filter (\i -> toUpperCase (fst i) `elem` acceptedKeys)

-- | Writes the given list of 'TagItem's into the specified handle as
-- ID3v1 tag.
--
-- Some tag items may be truncated or dropped completely if they have
-- an unsupported key.
writeId3v1Tag :: (TagItem a) => Handle -> [a] -> IO ()
writeId3v1Tag handle items =
    let tagData = getId3v1TagData items in
    do
        infoM "Id3v1Tag.writeId3v1Tag'" "Writing ID3v1 tag..."
        BS.hPut handle tagData

-- | Drops pairs with zero-length value from a given list.
dropEmpties :: [(String, String)] -> [(String, String)]
dropEmpties = filter (not . null . snd)

-- | Gets the track number from a given ByteString image of an ID3v1 tag,
-- if it is set.
getTrackNumber :: BS.ByteString -> Maybe Int
getTrackNumber id3data =
    if b1 == 0 && b2 /= 0 then Just (fromEnum b2) else Nothing
    where
        b1 = id3data `BS.index` trackNumberOffset - 1
        b2 = id3data `BS.index` trackNumberOffset

dropZeroes :: BS.ByteString -> BS.ByteString
dropZeroes = BS.filter (/= 0)

slice :: BS.ByteString -> Int -> Int -> BS.ByteString
slice bs length offset = BS.take length (BS.drop offset bs)

sliceWithoutZeroes :: BS.ByteString -> Int -> Int -> BS.ByteString
sliceWithoutZeroes = slice . dropZeroes

getTr :: BS.ByteString -> Maybe Int
getTr comment =
    if b1 == 0 && b2 /= 0 then Just (fromEnum b2) else Nothing
    where
        b1 = comment `BS.index` 28
        b2 = comment `BS.index` 29

getId3v1 :: Get [TextTagItem]
getId3v1 = do
    preamb <- getByteString (BS.length sigId3v1)
    when (preamb /= sigId3v1) (fail "Invalid ID3v1 preamble")
    title <- getByteString titleLength
    artist <- getByteString artistLength
    album <- getByteString albumLength
    year <- getByteString yearLength
    rawComment <- getByteString commentLength
    genreCode <- getWord8
    let genre = if genreCode > 0
            then Just (getGenreName (fromIntegral genreCode))
            else Nothing
        (trackNumber, comment) = case getTr rawComment of
            Just n -> (Just n, BS.take 28 comment)
            Nothing -> (Nothing, comment)
        decode str = BSC.unpack $ dropZeroes str
        items = [TextTagItem titleKey (decode title),
                 TextTagItem artistKey (decode artist),
                 TextTagItem albumKey (decode album),
                 TextTagItem yearKey (decode year),
                 TextTagItem commentKey (decode comment)]
        itemsWithGenre = case genre of
            Just genre -> TextTagItem genreKey genre : items
            Nothing -> items
        itemsWithTrackNumber = case trackNumber of
            Just n -> TextTagItem trackNumberKey (show n) : items
            Nothing -> items
    return itemsWithTrackNumber

readId3v1Values :: BS.ByteString -> [(String, String)]
readId3v1Values id3data =
    let preamb = BS.take (BS.length sigId3v1) id3data
    in if preamb /= sigId3v1
        then throw (TagsFormatException "Invalid ID3v1 preamble")
        else dropEmpties finalList
    where
        getValue length offset = BSC.unpack $ sliceWithoutZeroes id3data length offset
        title = getValue titleLength titleOffset
        artist = getValue artistLength artistOffset
        album = getValue albumLength albumOffset
        year = getValue yearLength yearOffset
        comment = getValue commentLength commentOffset
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
        finalList = case getTrackNumber id3data of
            Just n -> (trackNumberKey, toTwoDigit n) : listWithGenre
            Nothing -> listWithGenre

readId3v1Tag :: BS.ByteString -> [TextTagItem]
readId3v1Tag id3data = [TextTagItem k v | (k, v) <- values]
    where
        values = readId3v1Values id3data

-- | Reads an ID3v1 tag from a specified handle.
readId3v1Tag' :: Handle -> IO [TextTagItem]
readId3v1Tag' handle = do
    id3data <- BS.hGet handle id3v1Length
    return (readId3v1Tag id3data)
