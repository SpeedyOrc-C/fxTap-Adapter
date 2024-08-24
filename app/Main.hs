module Main where

import Data.ByteString.Lazy qualified as BL

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Parsec (parse)
import Data.Binary.Put (runPut)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeDirectory)
import System.Environment (getArgs)

import Osu.Parser (pOsu)
import FxTap ( FxTapCompatible(toFxTap), putFxTap )

debug parser = do
    t <- readFile "d.txt"
    print $ parse parser "" t

main :: IO ()
main = do
    args <- getArgs
    setLocaleEncoding utf8

    case args of

        [path] -> do
            raw <- readFile path

            case parse pOsu "" raw of

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

                    let fxTap = toFxTap osu

                    BL.writeFile outputPath (runPut $ putFxTap fxTap)
                    exitSuccess

        _ -> do
            putStrLn "Please provide an osu!mania beatmap."
            exitFailure
