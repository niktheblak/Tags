module Tags.Id3v2.TextType where

import Data.Char
import Tags.StringUtils

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

toTextType :: String -> Maybe TextType
toTextType str =
    read canonized
    where
        canonized = concat caps
        caps = map capitalize ws
        ws = words str

textFrameKey :: TextType -> String
textFrameKey = toUpperCase . separateByCap . show
