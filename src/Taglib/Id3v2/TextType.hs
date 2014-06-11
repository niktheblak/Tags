module Taglib.Id3v2.TextType where

import Data.Char
import Taglib.StringUtils

customTextFrameCode :: String
customTextFrameCode = "TXXX"

customTextFrameKey :: String
customTextFrameKey = "CUSTOM"

data TextType =
    Album |
    OriginalAlbum |
    ContentGroup |
    Title |
    Subtitle |
    Tracknumber |
    Artist |
    Performer |
    Conductor |
    Interpreter |
    OriginalArtist |
    PartOfASet |
    SetSubtitle |
    Isrc | 
    Lyrics |
    OriginalLyrics |
    Composer |
    MusicianCredits |
    InvolvedPeople |
    Encoded |
    Bpm |
    Length |
    Key |
    Language |
    Genre |
    FileType |
    MediaType |
    Copyright |
    Producer |
    Publisher |
    Owner |
    RadioStationName |
    RadioStationOwner |
    OriginalFilename |
    PlaylistDelay |
    EncodingTime |
    OriginalReleaseTime |
    RecordingTime
    deriving (Eq, Ord, Show, Read)

{- data TextTypeOld =
    TALB | TBPM | TCOM | TCON | TCOP | TDAT | TDLY | TENC | TEXT |
    TFLT | TIME |
    TIT1 | TIT2 | TIT3 |
    TIPL | TKEY | TLAN | TLEN | TMED | TMCL |
    TOAL | TOFN | TOLY | TOPE | TORY | TOWN |
    TPE1 | TPE2 | TPE3 | TPE4 |
    TPRO | TRSN | TRSO |
    TPOS | TPUB | TRCK | TRDA |
    TDEN | TDOR | TDRC | TSST | TSRC
    deriving (Show, Eq) -}

{- textTypeCode :: TextType -> String
textTypeCode = show -}

toTextType :: String -> Maybe TextType
toTextType str =
    read canonized
    where
        canonized = concat caps
        caps = map capitalize ws
        ws = words str

{- toTextType :: String -> Maybe TextType
toTextType str =
    case toUpperCase str of
        "ALBUM" -> Just Album
        "ORIGINAL ALBUM" -> Just OriginalAlbum
        "CONTENT GROUP" -> Just ContentGroup
        "TITLE" -> Just Title
        "SUBTITLE" -> Just Subtitle
        "TRACKNUMBER" -> Just Tracknumber
        "ARTIST" -> Just Artist
        "PERFORMER" -> Just Performer
        "CONDUCTOR" -> Just Conductor
        "INTERPRETER" -> Just Interpreter
        "ORIGINAL ARTIST" -> Just OriginalArtist
        "PART OF A SET" -> Just PartOfASet
        "SET SUBTITLE" -> Just SetSubtitle
        "ISRC" -> Just Isrc
        "LYRICS" -> Just Lyrics
        "ORIGINAL LYRICS" -> Just OriginalLyrics
        "COMPOSER" -> Just Composer
        "MUSICIAN CREDITS" -> Just MusicianCredits
        "INVOLVED PEOPLE" -> Just InvolvedPeople
        "ENCODED" -> Just Encoded
        "BPM" -> Just Bpm
        "LENGTH" -> Just Length
        "KEY" -> Just Key
        "LANGUAGE" -> Just Language
        "GENRE" -> Just Genre
        "FILE TYPE" -> Just FileType
        "MEDIA TYPE" -> Just MediaType
        "COPYRIGHT" -> Just Copyright
        "PRODUCER" -> Just Producer
        "PUBLISHER" -> Just Publisher
        "OWNER" -> Just Owner
        "RADIO STATION NAME" -> Just RadioStationName
        "RADIO STATION OWNER" -> Just RadioStationOwner
        "ORIGNAL FILENAME" -> Just OriginalFilename
        "PLAYLIST DELAY" -> Just PlaylistDelay
        "ENCODING TIME" -> Just EncodingTime
        "ORIGINAL RELEASE TIME" -> Just OriginalReleaseTime
        "RECORDING TIME" -> Just RecordingTime
        otherwise -> Nothing -}

textFrameKey :: TextType -> String
textFrameKey = toUpperCase . separateByCap . show
{- getTextFrameKey t =
    case t of
        TALB -> "ALBUM"
        TOAL -> "ORIGINAL ALBUM"
        TIT1 -> "CONTENT GROUP"
        TIT2 -> "TITLE"
        TIT3 -> "SUBTITLE"
        TRCK -> "TRACKNUMBER"
        TPE1 -> "ARTIST"
        TPE2 -> "PERFORMER"
        TPE3 -> "CONDUCTOR"
        TPE4 -> "INTERPRETER"
        TOPE -> "ORIGINAL ARTIST"
        TPOS -> "PART OF A SET"
        TSST -> "SET SUBTITLE"
        TSRC -> "ISRC"
        TEXT -> "LYRICS"
        TOLY -> "ORIGINAL LYRICS"
        TCOM -> "COMPOSER"
        TMCL -> "MUSICIAN CREDITS"
        TIPL -> "INVOLVED PEOPLE"
        TENC -> "ENCODED"
        TBPM -> "BPM"
        TLEN -> "LENGTH"
        TKEY -> "KEY"
        TLAN -> "LANGUAGE"
        TCON -> "GENRE"
        TFLT -> "FILE TYPE"
        TMED -> "MEDIA TYPE"
        TCOP -> "COPYRIGHT"
        TPRO -> "PRODUCER"
        TPUB -> "PUBLISHER"
        TOWN -> "OWNER"
        TRSN -> "RADIO STATION NAME"
        TRSO -> "RADIO STATION OWNER"
        TOFN -> "ORIGNAL FILENAME"
        TDLY -> "PLAYLIST DELAY"
        TDEN -> "ENCODING TIME"
        TDOR -> "ORIGINAL RELEASE TIME"
        TDRC -> "RECORDING TIME"
        otherwise -> show t -}
