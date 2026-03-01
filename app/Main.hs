{-# LANGUAGE LambdaCase #-}

module Main where

import Data.ByteString.Lazy qualified as BL

import Control.Applicative (Alternative ((<|>)), optional, (<**>))
import Control.Monad (when)
import Data.Aeson (decode)
import Data.Beatmap.FxTap (FxTapCompatible (toFxTap), toFxTap)
import Data.Beatmap.FxTap.Checker (
    Explain (..),
    FxTapMessage (..),
    fxTapChecker,
    isError,
    runChecker,
 )
import Data.Beatmap.FxTap.Put (putFxTapBinary, putFxTapCHeader)
import Data.Beatmap.Malody (Malody)
import Data.Beatmap.Osu.Parser (parserOsu)
import Data.Binary.Put (runPut)
import Data.Char (toLower)
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative (
    Parser,
    eitherReader,
    execParser,
    flag',
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    short,
    strOption, completeWith,
 )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)
import Text.Parsec (parse)

data FxTapArgs
    = FxTapMain
        { outputType :: OutputType
        , inputPath :: FilePath
        , outputPath :: Maybe FilePath
        }
    | FxTapVersion

data OutputType = OutputBinary ByteOrder | OutputC String

main :: IO ()
main = do
    setLocaleEncoding utf8
    getConfig >>= _main

_main :: FxTapArgs -> IO ()
_main FxTapVersion = putStrLn "0.5.0.0"
_main (FxTapMain{outputType, inputPath, outputPath}) = do
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
        '.' : extension -> return $ Left $ "Not supported extension " ++ extension ++ "."
        _ -> error "Unreachable"

    case result of
        Left error' ->
            traverse_
                putStrLn
                [ red "[SYNTAX ERROR]"
                , error'
                , ""
                , "If you believe this is an error, report it here:"
                , "https://github.com/SpeedyOrc-C/fxTap-Adapter/issues"
                ]
                >> exitFailure
        Right fxTap -> do
            let extension = case outputType of
                    OutputBinary{} -> ".fxt"
                    OutputC{} -> ".fxt.h"

            let defaultOutputPath = dropExtension inputPath
            let outputPath' = dropExtension (fromMaybe defaultOutputPath outputPath) ++ extension
            let messages = runChecker fxTapChecker fxTap

            for_ messages $ \msg -> do
                putStr $ case msg of
                    FxTapWarning{} -> yellow "[WARNING] "
                    FxTapError{} -> red "[ERROR] "
                putStrLn $ explain msg

            when (any isError messages) exitFailure

            BL.writeFile outputPath' . runPut $ case outputType of
                OutputBinary byteOrder -> putFxTapBinary byteOrder fxTap
                OutputC symbolName -> putFxTapCHeader symbolName fxTap

            exitSuccess
  where
    red :: String -> String
    red x = "\x1b[31m" ++ x ++ "\x1b[0m"

    yellow :: String -> String
    yellow x = "\x1b[33m" ++ x ++ "\x1b[0m"

printVersion :: IO ()
printVersion = putStrLn "fxTap Adapter 0.5.0.0"

getConfig :: IO FxTapArgs
getConfig = execParser (info (pArgs <**> helper) fullDesc)

pArgs :: Parser FxTapArgs
pArgs =
    pMain <|> pVersion
  where
    pOutputBinary =
        OutputBinary
            <$> pEndian
                ( long "bin"
                    <> short 'b'
                    <> metavar "big|little"
                    <> completeWith ["big", "casio", "little", "windows", "macos", "linux"]
                    <> help "Generate a binary file with specified endianness."
                )
      where
        pEndian = option . eitherReader $ \case
            "b" -> Right BigEndian
            "big" -> Right BigEndian
            "casio" -> Right BigEndian
            "l" -> Right LittleEndian
            "little" -> Right LittleEndian
            "windows" -> Right LittleEndian
            "macos" -> Right LittleEndian
            "linux" -> Right LittleEndian
            _ -> Left "Invalid endianness"

    pOutputCHeader =
        fmap OutputC . strOption $
            ( long "c"
                <> short 'c'
                <> metavar "IDENTIFIER"
                <> help "Generate a C and a header file with the beatmap hardcoded, identified by a specified name"
            )

    pOutputType = pOutputBinary <|> pOutputCHeader

    pInputPath =
        strOption
            ( long "input"
                <> short 'i'
                <> metavar "PATH"
                <> help "Path to the beatmap to be converted"
            )

    pOutputPath =
        optional . strOption $
            ( long "output"
                <> short 'o'
                <> metavar "PATH"
                <> help "Output path"
            )

    pVersion =
        flag'
            FxTapVersion
            ( long "version"
                <> short 'v'
                <> help "Show version"
            )

    pMain :: Parser FxTapArgs
    pMain = FxTapMain <$> pOutputType <*> pInputPath <*> pOutputPath
