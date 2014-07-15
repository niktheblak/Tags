module Tags.Id3v2.Compatibility(Id3v24TextType,
                                tov24TextCode,
                                fromv24TextCode,
                                id3v24TextType,
                                textType) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Tags.Id3v2.TextType

data Id3v24TextType =
    TALB | TBPM | TCOM | TCON | TCOP | TDAT | TDLY | TENC | TEXT |
    TFLT | TIME |
    TIT1 | TIT2 | TIT3 |
    TIPL | TKEY | TLAN | TLEN | TMED | TMCL |
    TOAL | TOFN | TOLY | TOPE | TORY | TOWN |
    TPE1 | TPE2 | TPE3 | TPE4 |
    TPRO | TRSN | TRSO |
    TPOS | TPUB | TRCK | TRDA |
    TDEN | TDOR | TDRC | TSST | TSRC |
    ISRC
    deriving (Eq, Ord, Show, Read)
    
v24toid :: Id3v24TextType -> B.ByteString
v24toid = B.pack . show

idtov24 :: B.ByteString -> Maybe Id3v24TextType
idtov24 = read . B.unpack

tov24TextCode :: TextType -> B.ByteString
tov24TextCode = v24toid . id3v24TextType

fromv24TextCode :: B.ByteString -> Maybe TextType
fromv24TextCode str =
    case idtov24 str of
        Just t -> Just (textType t)
        Nothing -> Nothing
    
id3v24TextType :: TextType -> Id3v24TextType
id3v24TextType tt = id3v24TextTypeMappings M.! tt

textType :: Id3v24TextType -> TextType
textType tt =
    let filtered = M.filter (== tt) id3v24TextTypeMappings
        e = M.toList filtered
    in case length e of
        1 -> fst (head e)
        _ -> error "Unambiguous mapping of Id3v24TextType -> TextType."

id3v24TextTypeMappings :: M.Map TextType Id3v24TextType
id3v24TextTypeMappings = M.fromList
  [ (Album, TALB),
    (OriginalAlbum, TOAL),
    (ContentGroup, TIT1),
    (Title, TIT2),
    (Subtitle, TIT3),
    (Tracknumber, TRCK),
    (Artist, TPE1),
    (Performer, TPE2),
    (Conductor, TPE3),
    (Interpreter, TPE4),
    (OriginalArtist, TOPE),
    (PartOfASet, TPOS),
    (SetSubtitle, TSST),
    (Isrc, ISRC),
    (Lyrics, TEXT),
    (OriginalLyrics, TOLY),
    (Composer, TCOM),
    (MusicianCredits, TMCL),
    (InvolvedPeople, TIPL),
    (Encoded, TENC),
    (Bpm, TBPM),
    (Length, TLEN),
    (Key, TKEY),
    (Language, TLAN),
    (Genre, TCON),
    (FileType, TFLT),
    (MediaType, TMED),
    (Copyright, TCOP),
    (Producer, TPRO),
    (Publisher, TPUB),
    (Owner, TOWN),
    (RadioStationName, TRSN),
    (RadioStationOwner, TRSO),
    (OriginalFilename, TOFN),
    (PlaylistDelay, TDLY),
    (EncodingTime, TDEN),
    (OriginalReleaseTime, TDOR),
    (RecordingTime, TDRC) ]
