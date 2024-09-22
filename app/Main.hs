module Main where

import Data.ByteString.Lazy qualified as BL

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Parsec (parse)
import Data.Foldable (traverse_)
import Data.Binary.Put (runPut)
import Control.Monad (unless)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeDirectory)
import System.Environment (getArgs)

import Osu.Parser (pOsu)
import FxTap ( FxTapCompatible(toFxTap), putFxTap, fxTapPutCheck, FxTapPutMessage (FxTapPutMessage) )

process :: String -> String -> IO ()
process raw path = case parse pOsu "" raw of
    Left parseError -> do
        putStrLn "Bad beatmap."
        putStrLn "If you believe this is an error, report to us."
        putStrLn ""
        print parseError
        exitFailure

    Right osu -> do
        let outputPath =
                takeDirectory path ++ "/" ++
                takeBaseName path ++ ".fxt"

            fxTap = toFxTap osu

            FxTapPutMessage warnings errors = fxTapPutCheck fxTap

        unless (null warnings) $ do
            putStrLn "Warning(s)"
            traverse_ print warnings
            putStrLn ""

        unless (null errors) $ do
            putStrLn "Error(s)"
            traverse_ print errors
            exitFailure

        BL.writeFile outputPath (runPut $ putFxTap fxTap)
        exitSuccess

main :: IO ()
main = do
    args <- getArgs
    setLocaleEncoding utf8

    case args of
        [path] -> do
            raw <- readFile path
            process raw path
        _ -> do
            putStrLn "Please provide an osu!mania beatmap."
            exitFailure
