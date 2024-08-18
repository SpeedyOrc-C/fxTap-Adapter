module Main where

import Text.Parsec (parse)

debug parser = do
    t <- readFile "d.txt"
    print $ parse parser "" t

main :: IO ()
main = putStrLn "Hello, Haskell!"
