module Taglib.Id3v2.URLType where

customURLFrameCode :: String
customURLFrameCode = "WXXX"

customURLFrameKey :: String
customURLFrameKey = "CUSTOM URL"

-- | ID3v2 URL frame types.
data URLType =
    WCOM | WCOP | WOAF | WOAR | WOAS | WORS | WPAY | WPUB
    deriving (Show, Eq)

-- | Gets an english text representation of an URL frame type that can be used
-- as an item key in other tag formats.
getURLFrameKey :: URLType -> String
getURLFrameKey t =
    case t of
        WCOM -> "COMMERCIAL INFORMATION"
        WCOP -> "COPYRIGHT"
        WOAF -> "AUDIO FILE HOMEPAGE"
        WOAR -> "ARTIST HOMEPAGE"
        WOAS -> "SOURCE HOMEPAGE"
        WORS -> "RADIO STATION HOMEPAGE"
        WPAY -> "PAYMENT"
        WPUB -> "PUBLISHER"
