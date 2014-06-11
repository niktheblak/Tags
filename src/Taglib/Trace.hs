-- | A module that considerably expands the functionality of the standard
-- 'Debug.Trace' module.
module Taglib.Trace(TraceLevel,
                     traceNone,
                     traceError,
                     traceWarning,
                     traceInformation,
                     traceDebug,
                     showTraceLevel,
                     readTraceLevel,
                     trace,
                     trace',
                     traceIf,
                     traceIO,
                     traceIOIf) where

import Data.IORef
import qualified Debug.Trace as DTrace
import System.IO
import System.IO.Error
import System.IO.Unsafe
import System.Time
import System.Locale
import Taglib.StringUtils

-- | Defines the trace level. Trace leves are mutually inclusive i.e. level
-- WARNING emits both ERROR and WARNING messages.
--
-- Trace levels are:
--
-- [@NONE@] No trace messages are emitted.
--
-- [@ERROR@] Program errors.
--
-- [@WARNING@] Possibly erraneous situations or other unwanted behavior.
--
-- [@INFORMATION@] Informative messages describing the normal execution of
-- the program.
--
-- [@DEBUG@] Fine-grained information about program execution that may be
-- useful in debugging.
type TraceLevel = Int

-- | No trace messages are emitted.
traceNone :: TraceLevel
traceNone = 0

-- | Program errors.
traceError :: TraceLevel
traceError = 1

-- | Possibly erraneous situations or other unwanted behavior.
traceWarning :: TraceLevel
traceWarning = 2

-- | Informative messages describing the normal execution of the program.
traceInformation :: TraceLevel
traceInformation = 3

-- | Fine-grained information about program execution that may be useful in
-- debugging.
traceDebug :: TraceLevel
traceDebug = 4

frmtStr = "%d.%m.%y %H:%M:%S"

-- | Gets the current trace level of the system.
currentTraceLevel :: TraceLevel
currentTraceLevel = unsafePerformIO getTraceLevel

showTraceLevel :: TraceLevel -> String
showTraceLevel lev =
    case lev of
        0 -> "NONE"
        1 -> "ERROR"
        2 -> "WARNING"
        3 -> "INFORMATION"
        4 -> "DEBUG"
        x -> "TRACELEVEL " ++ show x

readTraceLevel :: String -> TraceLevel
readTraceLevel str =
    case toUpperCase str of
        "ERROR" -> traceError
        "WARNING" -> traceWarning
        "INFORMATION" -> traceInformation
        "DEBUG" -> traceDebug
        otherwise -> traceNone

levelCheck :: TraceLevel -> Bool
levelCheck level = level <= currentTraceLevel

-- | Emits a trace message.
trace :: TraceLevel -> String -> a -> a
trace lev msg a1 = trace' lev "" msg a1

-- | Emits a conditional trace message.
--
-- This function is useful for e.g. writing trace warnings about a parameter
-- being in illegal range etc.
traceIf :: Bool  -- ^ Whether to write the trace message.
    -> TraceLevel  -- ^ Tracelevel of the message.
    -> String -- ^ The trace message.
    -> a -- ^ This value is returned after writing the trace message.
    -> a
traceIf b lev msg a1 = if b then trace' lev "" msg a1 else a1

indentNewLines :: String -> String
indentNewLines str =
    let lns = lines str in
    if length lns > 1
        then foldl (\ln1 ln2 -> concat [ln1, "\n\t", ln2]) "" lns
        else str

-- | Emits a trace message with an optional message source specifier.
{-# NOINLINE trace' #-}
trace' :: TraceLevel -> String -> String -> a -> a
trace' lev src msg a1 =
    if levelCheck lev
        then let finalMsg = unsafePerformIO (createFinalMsg lev src msg)
            in DTrace.trace finalMsg a1
        else a1
    {- where
        finalMsg =
            concat [time,
            " ", showTraceLevel lev, ": ",
            (if length src > 0 then concat ["[", src, "] "] else " "),
            indentNewLines msg]
            where
                time = unsafePerformIO (do
                    ctime <- getClockTime
                    calTime <- toCalendarTime ctime
                    return (formatCalendarTime defaultTimeLocale frmtStr calTime)) -}

createFinalMsg :: TraceLevel -> String -> String -> IO String
createFinalMsg lev src msg = do
    ctime <- getClockTime
    calTime <- toCalendarTime ctime
    let time = formatCalendarTime defaultTimeLocale frmtStr calTime
    return (concat [time,
            " ", showTraceLevel lev, ": ",
            (if length src > 0 then concat ["[", src, "] "] else " "),
            indentNewLines msg])

traceIO' :: TraceLevel -> String -> IO ()
traceIO' lev msg = do
    str <- createFinalMsg lev "" msg
    writeTraceMessage str

-- | Convinience function for emitting a trace message with an optional message
-- source specifier within the IO monad.
traceIO :: TraceLevel -- ^ Tracelevel of the message.
    -> String -- ^ Message source.
    -> String -- ^ The trace message.
    -> IO ()
traceIO lev src msg =
    if levelCheck lev
        then do
            str <- createFinalMsg lev src msg
            writeTraceMessage str
        else return ()

-- | Convinience function for conditionally emitting a trace message with an
-- optional message source specifier within the IO monad.
--
-- This function is useful for e.g. writing trace warnings about a parameter
-- being in illegal range etc.
traceIOIf :: Bool -- ^ Whether to write the trace message.
    -> TraceLevel -- ^ Tracelevel of the message.
    -> String -- ^ Message source.
    -> String -- ^ The trace message.
    -> IO ()
traceIOIf b lev src msg =
    if b then traceIO lev src msg else return ()

writeTraceMessage :: String -> IO ()
writeTraceMessage msg =
    return (DTrace.trace msg ())
    -- putStrLn str

configFileName :: String
configFileName = "trace.ini"

parseLevel :: String -> TraceLevel
parseLevel str =
    let (start, end) = break (\c -> c == '=') str
        trimmedStart = trim start
        trimmedEnd = trim (tail end) in
    if trimmedStart == "TRACELEVEL"
        then readTraceLevel trimmedEnd
        else error ("Syntax error: " ++ str)

getTraceLevel :: IO TraceLevel
getTraceLevel =
    catch (do
        DTrace.trace ("Getting trace level from file " ++ configFileName) return ()
        handle <- openFile configFileName ReadMode
        line <- hGetLine handle
        hClose handle
        let lev = parseLevel line
        DTrace.trace ("Using trace level " ++ showTraceLevel lev) return lev)
        (\e -> if isDoesNotExistError e
            then DTrace.trace
                ("Settings file " ++ configFileName ++ " does not exist; tracing disabled.")
                (return traceNone)
            else ioError e)

