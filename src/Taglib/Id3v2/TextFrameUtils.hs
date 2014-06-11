module Taglib.Id3v2.TextFrameUtils where

import Taglib.Id3v2.Encoding
import Taglib.Id3v2.Frames
import Taglib.Id3v2.TextType

createTextFrame :: TextType -> String -> Frame
createTextFrame tp text = TextFrame tp text defaultEncoding

createTitleFrame :: String -> Frame
createTitleFrame str = TextFrame Title str defaultEncoding

createArtistFrame :: String -> Frame
createArtistFrame str = TextFrame Artist str defaultEncoding

createAlbumFrame :: String -> Frame
createAlbumFrame str = TextFrame Album str defaultEncoding
