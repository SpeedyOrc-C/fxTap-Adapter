{-# LANGUAGE LambdaCase #-}
module FxTap where
import Data.Binary.Put (Put, putStringUtf8, putInt16le)
import Data.Foldable (for_)
import Control.Monad ( replicateM_ )

class FxTapCompatible beatmap where
    toFxTap :: beatmap -> FxTap

data FxTap = FxTap {
    title :: String,
    artist :: String,
    -- 1st dimension : columns
    -- 2nd dimension : notes
    notesColumns :: [[Note]]
} deriving Show

data Note
    = Tap { accumulatedStartTime :: Integer }
    | Hold { accumulatedStartTime :: Integer, duration :: Integer }
    deriving Show

putFxTap :: FxTap -> Put
putFxTap FxTap {title, artist, notesColumns} = do
    putStringUtf8 "fx4K-beatmap-v01"

    let str32 :: String -> String
        str32 string =
            if length string <= 32 then
                string ++ replicate (32 - length string) ' '
            else
                take 32 string

    putStringUtf8 $ str32 title
    putStringUtf8 $ str32 artist

    for_ notesColumns $ \notesColumn -> do
        putInt16le . fromIntegral $ length notesColumn

    replicateM_ (8 - length notesColumns) $ do
        putInt16le 0

    for_ notesColumns $ \notesColumn -> do
        for_ notesColumn $ \case

            Tap {accumulatedStartTime} -> do
                putInt16le . fromInteger $ accumulatedStartTime
                putInt16le 0

            Hold {accumulatedStartTime, duration} -> do
                putInt16le . fromInteger $ accumulatedStartTime
                putInt16le . fromInteger $ duration
