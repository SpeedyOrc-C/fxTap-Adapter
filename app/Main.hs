module Main where

import Data.ByteString.Lazy qualified as BL

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Parsec (parse)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_, traverse_)
import Data.Binary.Put (runPut)
import Data.Aeson (decode)
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)
import System.Environment (getArgs)

import Data.Beatmap.FxTap ( FxTapCompatible(toFxTap), toFxTap )
import Data.Beatmap.FxTap.Put ( putFxTapBinary, putFxTapCHeader )
import Data.Beatmap.FxTap.Checker ( fxTapChecker, runChecker, FxTapMessage (..), isError, Explain (..) )
import Data.Beatmap.Osu.Parser (parserOsu)
import Data.Beatmap.Malody (Malody)

data OutputType = OutputBinary | OutputCHeader String

red :: String -> String
red str = "\x1b[31m" ++ str ++ "\x1b[0m"

yellow :: String -> String
yellow str = "\x1b[33m" ++ str ++ "\x1b[0m"

rock :: OutputType -> FilePath -> Maybe FilePath -> IO ()
rock outputType inputPath maybeOutputPath = do
    result <- case map toLower (takeExtension inputPath) of
        ".osu" -> do
            raw <- readFile inputPath
            return $ case parse parserOsu "" raw of
                Left error' -> Left (show error')
                Right osu -> Right (toFxTap osu)

        ".mc" -> do
            raw <- BL.readFile inputPath
            return $ case decode raw :: Maybe Malody of
                Nothing -> Left "Cannot parse this Malody beatmap."
                Just malody -> Right (toFxTap malody)

        "" -> return $ Left "No extension found, cannot determine the file type."

        extension -> return $ Left $ "Not supported extension " ++ tail extension ++ "."

    case result of
        Left error' -> do
            putStrLn $ red "[SYNTAX ERROR]"
            putStrLn error'
            putStrLn ""
            putStrLn "If you believe this is an error, report it here:"
            putStrLn "https://github.com/SpeedyOrc-C/fxTap-Adapter/issues"
            exitFailure

        Right fxTap -> do
            let extension = case outputType of
                    OutputBinary -> ".fxt"
                    OutputCHeader {} -> ".fxt.h"

            let defaultOutputPath = dropExtension inputPath
            let outputPath = dropExtension (fromMaybe defaultOutputPath maybeOutputPath) ++ extension
            let messages = runChecker fxTapChecker fxTap

            for_ messages $ \message -> do
                putStr $ case message of
                    FxTapWarning {} -> yellow "[WARNING] "
                    FxTapError {} -> red "[ERROR] "
                putStrLn $ explain message

            when (any isError messages) exitFailure

            BL.writeFile outputPath . runPut $ case outputType of
                OutputBinary -> putFxTapBinary fxTap
                OutputCHeader symbolName -> putFxTapCHeader symbolName fxTap

            exitSuccess

printVersion :: IO ()
printVersion = putStrLn "fxTap Adapter 0.5.0.0"

printHelp :: IO ()
printHelp =
    traverse_ putStrLn
    [ "Usage: fxta [-ch <symbol_name>] <beatmap_path> [<output_path>]"
    , ""
    , "beatmap_path : Path to osu! or Malody beatmap."
    , "output_path : Path to the converted beatmap. If omitted, it is the same as <beatmap_path>."
    , "symbol_name : If provided, the output will be a C header file, with a initialised variable called <symbol_name>."
    ]

printInvalidArguments :: IO ()
printInvalidArguments = do
    putStrLn "Invalid arguments. Please use -h or --help for more information."
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    setLocaleEncoding utf8

    case args of
        ["-v"] -> printVersion
        ["--version"] -> printVersion
        ["-h"] -> printHelp
        ["--help"] -> printHelp

        ["-ch", symbolName, inputPath] ->
            rock (OutputCHeader symbolName) inputPath Nothing

        ["-ch", symbolName, inputPath, outputFileName] ->
            rock (OutputCHeader symbolName) inputPath (Just outputFileName)

        [inputPath] ->
            rock OutputBinary inputPath Nothing

        [inputPath, outputFileName] ->
            rock OutputBinary inputPath (Just outputFileName)

        _ -> printInvalidArguments
