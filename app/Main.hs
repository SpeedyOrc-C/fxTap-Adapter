module Main where

import Control.Monad (when)
import Data.Beatmap.FxTap (FxTap, FxTapCompatible (toFxTap), toFxTap)
import Data.Beatmap.FxTap.Checker (
    Explain (..),
    FxTapMessage (..),
    checkFxTap,
    isError,
 )
import Data.Beatmap.FxTap.Put (putFxTapBinary, putFxTapCHeader)
import Data.Beatmap.Malody (parseMalody)
import Data.Beatmap.Osu.Parser (parseOsu)
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy qualified as BL
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import FxTapAdapter (FxTapArgs (..), OutputType (..), getFxtaArgs)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)

main :: IO ()
main = do
    setLocaleEncoding utf8
    args <- getFxtaArgs
    case args of
        FxTapVersion -> do
            putStrLn "1.0.1.1"
            exitSuccess
        FxTapMain{outputType, inputPath, outputPath} -> do
            syntaxErrorOrBeatmap <- loadBeatmapFromPath inputPath
            case syntaxErrorOrBeatmap of
                Left syntaxError -> do
                    printSyntaxError syntaxError
                    exitFailure
                Right beatmap -> do
                    let messages = checkFxTap beatmap
                    printFxTapMessages messages
                    when (any isError messages) exitFailure
                    writeOutput inputPath outputPath outputType beatmap

loadBeatmapFromPath :: FilePath -> IO (Either String FxTap)
loadBeatmapFromPath path = case map toLower (takeExtension path) of
    ".osu" -> do
        raw <- readFile path
        return $ case parseOsu raw of
            Left error' -> Left (show error')
            Right osu -> Right (toFxTap osu)
    ".mc" -> do
        raw <- BL.readFile path
        return $ case parseMalody raw of
            Left error' -> Left (show error')
            Right malody -> Right (toFxTap malody)
    "" -> return $ Left "No extension found, can't determine the file type."
    '.' : extension -> return $ Left $ "Not supported extension " ++ extension ++ "."
    _ -> error "Unreachable"

printSyntaxError :: String -> IO ()
printSyntaxError error' = do
    putStrLn "\x1b[31m[ERROR]\x1b[0m Syntax"
    putStrLn error'
    putStrLn ""
    putStrLn "If you believe this is an error, report it here:"
    putStrLn "https://github.com/SpeedyOrc-C/fxTap-Adapter/issues"

printFxTapMessages :: [FxTapMessage] -> IO ()
printFxTapMessages messages = do
    for_ messages $ \message -> do
        putStr $ case message of
            FxTapWarning{} -> "\x1b[33m[WARNING]\x1b[0m "
            FxTapError{} -> "\x1b[31m[ERROR]\x1b[0m "
        putStrLn (explain message)

writeOutput :: FilePath -> Maybe FilePath -> OutputType -> FxTap -> IO ()
writeOutput inputPath outputPath outputType beatmap =
    BL.writeFile outputPath' . runPut $
        case outputType of
            OutputBinary byteOrder ->
                putFxTapBinary byteOrder beatmap
            OutputC symbolName ->
                putFxTapCHeader symbolName beatmap
  where
    extension = case outputType of OutputBinary{} -> ".fxt"; OutputC{} -> ".fxt.h"
    defaultOutputPath = dropExtension inputPath
    outputPath' = dropExtension (fromMaybe defaultOutputPath outputPath) ++ extension
