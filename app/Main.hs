module Main where

import Data.ByteString.Lazy qualified as BL

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Text.Parsec (parse)
import Data.Binary.Put (runPut)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeBaseName, takeDirectory)
import System.Environment (getArgs)

import Osu.Parser (pOsu)
import Osu.Fx4K (wFx4K)

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

                Right beatmap -> do
                    let outputPath =
                            takeDirectory path ++ "/" ++
                            takeBaseName path ++ ".f4k"
                    BL.writeFile outputPath (runPut $ wFx4K beatmap)
                    exitSuccess

        _ -> do
            putStrLn "Please provide an osu!mania beatmap."
            exitFailure
