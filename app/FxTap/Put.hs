{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module FxTap.Put where

import Data.Foldable (for_)
import Data.Binary.Put (Put, putStringUtf8, putInt16le, putDoublele, putCharUtf8)
import Control.Monad ( replicateM_ )

import FxTap (FxTap(..), Note (..))

{-| Generate a binary representation of a fxTap beatmap.

All integers in fxTap's binary beatmap is in LITTLE endian.

00-0F   A header indicates the file version.
        "fx4K-beatmap-v01"

10-2F   Beatmap's title
30-4F   Beatmap's artist

        Unused memory are filled with zeros.
        The last character must be terminated by 0.
        So it allows 31 characters at most.

50-6F   Number of notes in columns 1 to 8.
        Each number has 2 bytes.
        This also indicates the number of keys in the gameplay.

        Examples:

        [12, 34, 56, 78, 0, 0, 0, 0]
            4K with 12 notes in column 1, 34 notes in column 2...
        [98, 76, 54, 32, 10, 42, 0, 0]
            6K with 98 notes in column 1, 42 notes in column 6

70-77   Overall difficulty (IEEE floating number)

80-     All notes. The structure of a single note is:

        00~01   Accumulated start time
        02~03   Duration

        If the duration is zero, it's a tap note.
        Otherwise it's a hold note.

        Accumulated start time is used to reduce the memory needed,
        since we got a really small RAM on CASIO calculators.

        Examples:

        For [100, 100, 200, 100, 300, 100], actual time of notes are
            [100, 200, 400, 500, 800, 900].

        For [1, 1, 1, 1, 1], actual times of notes are
            [1, 2, 3, 4, 5].

        This helps us keep the time of each note as small as possible.
        So that we don't need a large integer to store it.

        p.s. Duration doesn't affect the accumulated time.

        Notes are listed COLUMN BY COLUMN, not by time,
        which means you'll firstly see all notes in
        column 1, then column 2, then column 3, and so on.
        And the accumulated time in each column is calculated separately.
-}
putFxTap :: FxTap -> Put
putFxTap FxTap {..} = do
    putStringUtf8 "fx4K-beatmap-v01"

    let str32 :: String -> String
        str32 string =
            if length string <= 31 then
                string ++ replicate (32 - length string) '\0'
            else
                take 31 string ++ "\0"

    putStringUtf8 $ str32 title
    putStringUtf8 $ str32 artist

    for_ notesColumns $ \notesColumn -> do
        putInt16le . fromIntegral $ length notesColumn

    replicateM_ (16 - length notesColumns) $ do
        putInt16le 0

    putDoublele overallDifficulty

    replicateM_ 8 (putCharUtf8 '\0')

    for_ notesColumns $ \notesColumn -> do
        for_ notesColumn $ \case

            Tap {accumulatedStartTime} -> do
                putInt16le . fromInteger $ accumulatedStartTime
                putInt16le 0

            Hold {accumulatedStartTime, duration} -> do
                putInt16le . fromInteger $ accumulatedStartTime
                putInt16le . fromInteger $ duration
