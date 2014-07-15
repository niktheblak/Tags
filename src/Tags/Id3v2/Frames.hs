module Tags.Id3v2.Frames where

import qualified Data.ByteString as BS
import Data.Word
import Network.URI(URI(..),parseURIReference)
import Tags.ItemData
import Tags.Id3v2.Encoding
import Tags.Id3v2.PictureType
import Tags.Id3v2.TextType
import Tags.Id3v2.URLType

-- | An abstraction of an ID3v2 frame.
data Frame =
    -- | Text frame type with specified encoding.
    -- Frames that contain only textual information.
    TextFrame TextType String Encoding |
    -- | User-defined text frame type with specified encoding. Custom text frame
    -- type has a description field in addition to the content field.
    CustomTextFrame String String Encoding |
    -- | Picture frame type. Contains binary data of a picture.
    PictureFrame PictureProperties BS.ByteString |
    -- | URL frame type. Contains an URL.
    URLFrame URLType URI |
    -- | User-defined URL frame type.
    CustomURLFrame String URI
    deriving Eq

instance Show Frame where
    show (TextFrame tp text _) = textFrameKey tp ++ " = " ++ text
    show (CustomTextFrame descr text _) =
        customTextFrameCode ++ " (" ++ descr ++ ") = " ++ text
    show (CustomURLFrame descr uri) =
        customURLFrameCode ++ " (" ++ descr ++ ") = " ++ show uri
    show (PictureFrame _ arr) =
        pictureFrameKey ++ " = [binary data " ++ show (BS.length arr) ++ " bytes]" 

instance TagItem Frame where
    key (TextFrame tp _ _) = textFrameKey tp
    key (CustomTextFrame _ _ _) = customTextFrameKey
    key (URLFrame tp _) = getURLFrameKey tp
    key (CustomURLFrame _ _) = customURLFrameKey
    key (PictureFrame _ _) = pictureFrameKey

    value (TextFrame _ text _) = Text text
    value (CustomTextFrame descr text _) = Text text    
    value (URLFrame _ uri) = Text (show uri)
    value (CustomURLFrame _ uri) = Text (show uri)
    value (PictureFrame _ pdata) = Binary pdata

-- | Additional properties of an ID3v2 picture frame.
data PictureProperties = PictureProperties {
    -- | Encoding of the description text.
    textEncoding :: Encoding,
    -- | MIME type of the picture.
    mimeType :: String,
    -- | Type of the picture.
    pictureType :: PictureType,
    -- | Description of the picture.
    description :: String } deriving (Eq, Show)
