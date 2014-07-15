module Tags.Id3v2.CommonIO where

import qualified Data.ByteString.Char8 as BSC
import Data.Word
import System.IO
import Tags.BinaryIO
import Tags.Id3v2.Frames
import Tags.Id3v2.Encoding

writePreamble :: Handle -> String -> Int -> IO ()
writePreamble handle code size =
    let codeBytes = BSC.pack code
    in do
        BSC.hPut handle codeBytes
        write32LE handle (toEnum size)
        
writeText :: Handle -> Encoding -> String -> IO ()
writeText handle enc str = do
    writeWord8 handle (enctoid enc)
    writeData handle (encode enc str)
