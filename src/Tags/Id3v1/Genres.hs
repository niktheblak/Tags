module Tags.Id3v1.Genres(getGenreCode, getGenreName, id3GenreNames) where

import Data.List
import Tags.StringUtils

-- | Gets the corresponding ID3v1 genre code for the given genre name,
-- or @Nothing@ if the genre name couldn't be found.
--
-- The genre names are searched from 'id3GenreNames' with some level
-- of fuzziness, e.g. ignored case and punctuation.
getGenreCode :: String -> Maybe Int
getGenreCode name =
    case normalizedFormFind name id3GenreNames of
        Just idx -> Just idx
        Nothing -> case guessGenreCode name of
            Just idx -> Just idx
            Nothing -> Nothing

guessGenreCode :: String -> Maybe Int
guessGenreCode name =
    let normName = normalizedForm name in
    case normName of
        "ALTERNATIVEROCK" -> Just 40
        "ALTROCK" -> Just 40
        "GOTHICMETAL" -> Just 49
        _ -> if "ROCK" `isPrefixOf` normName then Just 78 else
            case indexOf "METAL" normName of
                Just _ -> Just 9
                Nothing -> Nothing

-- | Gets the name of the genre specified by the given ID3v1 genre code.
--
-- This function returns genre number 0 (Blues) if the genre code is lower
-- than zero or higher than the current genre count (127).
getGenreName :: Int -> String
getGenreName n
    | n >= 0 && n < length id3GenreNames = id3GenreNames !! n
    | otherwise = head id3GenreNames


-- | List of ID3v1 genres.
--
-- The genre list comprises of official ID3v1 genre list
-- (see <http://www.id3.org>) and Winamp extensions
-- (see <http://en.wikipedia.org/wiki/ID3>).
id3GenreNames :: [String]
id3GenreNames = [
    "Blues", -- The 'official' genres 
    "Classic Rock",
    "Country",
    "Dance",
    "Disco",
    "Funk",
    "Grunge",
    "Hip-Hop",
    "Jazz",
    "Metal", 
    "New Age",
    "Oldies",
    "Other",
    "Pop",
    "R&B",
    "Rap",
    "Reggae",
    "Rock",
    "Techno",
    "Industrial",
    "Alternative",
    "Ska",
    "Death Metal",
    "Pranks",
    "Soundtrack",
    "Euro-Techno",
    "Ambient",
    "Trip-Hop",
    "Vocal",
    "Jazz+Funk",
    "Fusion",
    "Trance",
    "Classical",
    "Instrumental",
    "Acid",
    "House",
    "Game",
    "Sound Clip",
    "Gospel",
    "Noise", 
    "AlternRock",
    "Bass",
    "Soul",
    "Punk",
    "Space",
    "Meditative",
    "Instrumental Pop",
    "Instrumental Rock",
    "Ethnic",
    "Gothic", 
    "Darkwave",
    "Techno-Industrial",
    "Electronic",
    "Pop-Folk",
    "Eurodance",
    "Dream",
    "Southern Rock",
    "Comedy",
    "Cult",
    "Gangsta",
    "Top",
    "Christian Rap",
    "Pop/Funk",
    "Jungle",
    "Native American",
    "Cabaret",
    "New Wave",
    "Psychadelic",
    "Rave",
    "Showtunes", 
    "Trailer",
    "Lo-Fi",
    "Tribal",
    "Acid Punk",
    "Acid Jazz",
    "Polka",
    "Retro",
    "Musical",
    "Rock & Roll",
    "Hard Rock",
    "Folk", -- Winamp extensions
    "Folk-Rock",
    "National Folk",
    "Swing",
    "Fast Fusion",
    "Bebob",
    "Latin",
    "Revival",
    "Celtic",
    "Bluegrass",
    "Avantgarde",
    "Gothic Rock",
    "Prog. Rock",
    "Psychedel. Rock",
    "Symph. Rock",
    "Slow Rock",
    "Big Band",
    "Chorus",
    "Easy Listening",
    "Acoustic", 
    "Humour",
    "Speech",
    "Chanson",
    "Opera",
    "Chamber Music",
    "Sonata",
    "Symphony",
    "Booty Bass",
    "Primus",
    "Porn Groove",
    "Satire",
    "Slow Jam",
    "Club",
    "Tango",
    "Samba",
    "Folklore",
    "Ballad",
    "Power Ballad",
    "Rhythmic Soul",
    "Freestyle",
    "Duet",
    "Punk Rock",
    "Drum Solo",
    "Acapella",
    "Euro-House",
    "Dance Hall" ]
