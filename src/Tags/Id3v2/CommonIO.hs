module Tags.Id3v2.CommonIO where

import Data.Word
import System.IO
import Tags.Ascii
import Tags.BinaryIO
import Tags.Id3v2.Frames
import Tags.Id3v2.Encoding

writePreamble :: Handle -> String -> Int -> IO ()
writePreamble handle code size =
    let codeBytes = toAscii code
    in do
        writeData handle codeBytes
        write32LE handle (toEnum size)
        
writeText :: Handle -> Encoding -> String -> IO ()
writeText handle enc str = do
    writeWord8 handle (enctoid enc)
    writeData handle (encode enc str)

{-

        @param output       output buffer to write to
        @param frameCode    ID3v2 frame code, e.g. APIC
        @param frameSize    size of the frame data in bytes
        @param flags        frame flags
    */
    def writePreamble(output: ByteBuffer,
            frameCode: String,
            frameSize: Int,
            flags: List[FrameFlags.Value]): Unit = {
        output.put(frameCode.getBytes("US-ASCII"))
        output.putInt(frameSize)
        output.putShort(FrameFlags.toShort(flags))
    }
    
    def writeText(output: ByteBuffer, encoding: Encoding.Value, text: String): Unit = {
        val encodedText = Encoder.encode(text, encoding)
        // Encoding specifier
        output.put(Encoding.toByte(encoding))
        // Encoded text
        output.put(encodedText)
        // Terminator, two zero bytes for UTF-16 and one zero for ISO and UTF-8.
        if (encoding == Encoding.UTF16 || encoding == Encoding.UTF16BE)
            output.putShort(0)
        else
            output.put(0: Byte)

-}
