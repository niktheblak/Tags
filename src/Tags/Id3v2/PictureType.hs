module Tags.Id3v2.PictureType where

-- | ID3v2 picture frame code.
pictureFrameCode :: String
pictureFrameCode = "APIC"

-- | English representation of the picture frame type that can be used
-- as an item key in other tag formats.
pictureFrameKey :: String
pictureFrameKey = "PICTURE"

-- | ID3v2 picture frame types.
data PictureType =
    -- | Other
    Other |
    -- | 32x32 pixels 'file icon' (PNG only)
    PngFileIcon |
    -- | Other file icon
    OtherFileIcon |
    -- | Cover (front)
    FrontCover |
    -- | Cover (back)
    BackCover |
    -- | Leaflet page
    Leaflet |
    -- | Media (e.g. label side of CD)
    Media |
    -- | Lead artist/lead performer/soloist
    LeadArtist |
    -- | Artist/performer
    Artist |
    -- | Conductor
    Conductor |
    -- | Band/Orchestra
    Band |
    -- | Composer
    Composer |
    -- | Lyricist/text writer
    Lyricist |
    -- | Recording Location
    RecordingLocation |
    -- | During recording
    DuringRecording |
    -- | During performance
    DuringPerformance |
    -- | Movie/video screen capture
    ScreenCapture |
    -- | A bright coloured fish (WTF?)
    Fish |
    -- | Illustration
    Illustration |
    -- | Band/artist logotype
    BandLogo |
    -- | Publisher/Studio logotype
    PublisherLogo
    deriving (Eq, Show)

pictureTypeCode :: PictureType -> Int
pictureTypeCode p =
    case p of
        Other               -> 0x0
        PngFileIcon         -> 0x1
        OtherFileIcon       -> 0x2
        FrontCover          -> 0x3
        BackCover           -> 0x4
        Leaflet             -> 0x5
        Media               -> 0x6
        LeadArtist          -> 0x7
        Artist              -> 0x8
        Conductor           -> 0x9
        Band                -> 0xA
        Composer            -> 0xB
        Lyricist            -> 0xC
        RecordingLocation   -> 0xD
        DuringRecording     -> 0xE
        DuringPerformance   -> 0xF
        ScreenCapture       -> 0x10
        Fish                -> 0x11
        Illustration        -> 0x12
        BandLogo            -> 0x13
        PublisherLogo       -> 0x14
