{-# LANGUAGE LambdaCase #-}
module Main where

import Data.ByteString.Lazy qualified as BL

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Parsec (parse)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)
import Data.Binary.Put (runPut)
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (dropExtension)
import System.Environment (getArgs)

import Osu.Parser (pOsu)
import FxTap ( FxTapCompatible(toFxTap), putFxTap )
import FxTap.Checker ( fxTapChecker, runChecker, FxTapMessage (..), isError )

process :: FilePath -> Maybe FilePath -> IO ()
process inputPath maybeOutputPath = do
    raw <- readFile inputPath
    case parse pOsu "" raw of
        Left parseError -> do
            putStrLn "Bad beatmap!"
            putStrLn "If you believe this is an error, report to us."
            putStrLn ""
            print parseError
            exitFailure

        Right osu -> do
            let
                defaultOutputPath =
                    dropExtension inputPath ++ ".fxt"

                outputPath = dropExtension
                    (fromMaybe defaultOutputPath maybeOutputPath) ++ ".fxt"

                fxTap = toFxTap osu

                messages = runChecker fxTapChecker fxTap

            for_ messages $ \message -> do
                putStrLn $ case message of
                    FxTapWarning {} -> "[WARNING]"
                    FxTapError {} -> "[ERROR]"
                putStr "    "; print message

            when (any isError messages) exitFailure

            BL.writeFile outputPath (runPut $ putFxTap fxTap)
            exitSuccess

main :: IO ()
main = do
    args <- getArgs
    setLocaleEncoding utf8

    case args of
        [inputPath] ->
            process inputPath Nothing

        [inputPath, outputFileName] ->
            process inputPath (Just outputFileName)

        _ -> do
            putStrLn "Please provide an osu!mania beatmap."
            exitFailure
