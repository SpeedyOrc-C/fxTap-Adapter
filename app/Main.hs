module Main where

import Data.ByteString.Lazy qualified as BL

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Parsec (parse)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.Binary.Put (runPut)
import Data.Aeson (decode)
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension, takeExtension)
import System.Environment (getArgs)

import Data.Beatmap.FxTap ( FxTapCompatible(toFxTap), toFxTap )
import Data.Beatmap.FxTap.Put ( putFxTap )
import Data.Beatmap.FxTap.Checker ( fxTapChecker, runChecker, FxTapMessage (..), isError, Explain (..) )
import Data.Beatmap.Osu.Parser (parserOsu)
import Data.Beatmap.Malody (Malody)

red :: String -> String
red str = "\x1b[31m" ++ str ++ "\x1b[0m"

yellow :: String -> String
yellow str = "\x1b[33m" ++ str ++ "\x1b[0m"

process :: FilePath -> Maybe FilePath -> IO ()
process inputPath maybeOutputPath = do
    result <- case map toLower (takeExtension inputPath) of
        ".osu" -> do
            raw <- readFile inputPath
            case parse parserOsu "" raw of
                Left error' -> return $ Left (show error')
                Right osu -> return $ Right (toFxTap osu)

        ".mc" -> do
            raw <- BL.readFile inputPath
            case decode raw :: Maybe Malody of
                Nothing -> return $ Left "Cannot parse this Malody beatmap."
                Just malody -> return $ Right (toFxTap malody)

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
            let defaultOutputPath = dropExtension inputPath ++ ".fxt"
            let outputPath = dropExtension (fromMaybe defaultOutputPath maybeOutputPath) ++ ".fxt"
            let messages = runChecker fxTapChecker fxTap

            for_ messages $ \message -> do
                putStr $ case message of
                    FxTapWarning {} -> yellow "[WARNING] "
                    FxTapError {} -> red "[ERROR] "
                putStrLn $ explain message

            when (any isError messages) exitFailure

            BL.writeFile outputPath (runPut $ putFxTap fxTap)
            exitSuccess

printVersion :: IO ()
printVersion = putStrLn "fxTap Adapter 0.4.0.1"

main :: IO ()
main = do
    args <- getArgs
    setLocaleEncoding utf8

    case args of
        ["-v"] -> printVersion
        ["--version"] -> printVersion

        [inputPath] ->
            process inputPath Nothing

        [inputPath, outputFileName] ->
            process inputPath (Just outputFileName)

        _ -> do
            putStrLn "Please provide an beatmap."
            exitFailure
