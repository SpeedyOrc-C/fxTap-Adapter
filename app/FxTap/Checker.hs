{-# LANGUAGE RecordWildCards #-}
module FxTap.Checker where

import Data.Function ((&))

import FxTap (FxTap (..), Note (..))

data FxTapWarning
    = TitleTrimmed
    | ArtistTrimmed
    | OverlappedHold {
        overlappedColumnIndex :: Integer,
        overlappedHoldIndex :: Integer,
        overlappedNotesCount :: Integer
    }
    deriving Show

data FxTapError
    = NoteIntervalTooLarge {
        overflowColumnIndex :: Integer,
        overflowNoteIndex :: Integer,
        overflowedValue :: Integer
    }
    | HoldDurationTooLarge {
        overflowColumnIndex :: Integer,
        overflowNoteIndex :: Integer,
        overflowedValue :: Integer
    }
    deriving Show

data FxTapMessage
    = FxTapWarning FxTapWarning
    | FxTapError FxTapError
    deriving Show

newtype FxTapChecker = FxTapChecker (FxTap -> [FxTapMessage])

titleChecker :: FxTapChecker
titleChecker = FxTapChecker $ \FxTap {title} ->
    [FxTapWarning TitleTrimmed | length title > 31]

artistChecker :: FxTapChecker
artistChecker = FxTapChecker $ \FxTap {artist} ->
    [FxTapWarning ArtistTrimmed | length artist > 31]

columnOverlapChecker :: [(Integer, Note)] -> [(Integer, Integer)]
columnOverlapChecker [] = []
columnOverlapChecker ((_, Tap {}):notes) = columnOverlapChecker notes
columnOverlapChecker ((noteIndex, Hold {duration}):notes) =
    if overlappedNotesCount == 0
        then columnOverlapChecker notes
        else (noteIndex, overlappedNotesCount) : columnOverlapChecker notes
    where
    overlappedNotesCount = notes
        -- Get following notes' accumulated start time
        & map (accumulatedStartTime . snd)
        -- Calculate the time difference
        -- between this hold note and the following notes
        & scanl1 (+)
        -- Find the overlapped notes
        & takeWhile (<= duration)
        -- Count how many are there
        & fromIntegral . length

overlapChecker :: FxTapChecker
overlapChecker = FxTapChecker $ \FxTap {notesColumns} -> do
    (columnIndex, notes) <- zip [0..] notesColumns
    (holdIndex, overlapCount) <- columnOverlapChecker (zip [0..] notes)

    return $ FxTapWarning $ OverlappedHold columnIndex holdIndex overlapCount

u16UpperBound :: Integer
u16UpperBound = (2 :: Integer) ^ (16 :: Integer) - 1

intervalOverflowChecker :: FxTapChecker
intervalOverflowChecker =  FxTapChecker $ \FxTap {notesColumns} -> do
    (columnIndex, notes) <- zip [0..] notesColumns
    (noteIndex, note) <- zip [0..] notes

    [FxTapError $
        NoteIntervalTooLarge columnIndex noteIndex (accumulatedStartTime note)
        | accumulatedStartTime note > u16UpperBound]

durationOverflowChecker :: FxTapChecker
durationOverflowChecker =  FxTapChecker $ \FxTap {notesColumns} -> do
    (columnIndex, notes) <- zip [0..] notesColumns
    (noteIndex, note) <- zip [0..] notes

    [FxTapError $
        HoldDurationTooLarge columnIndex noteIndex (duration note)
        | isHold note, duration note > u16UpperBound]

fxTapChecker :: FxTapChecker
fxTapChecker = mconcat
    [ titleChecker
    , artistChecker
    , overlapChecker
    , intervalOverflowChecker
    , durationOverflowChecker
    ]

runChecker :: FxTapChecker -> FxTap -> [FxTapMessage]
runChecker (FxTapChecker f) = f

isError :: FxTapMessage -> Bool
isError FxTapError {} = True
isError _ = False

isHold :: Note -> Bool
isHold Hold {} = True
isHold _ = False

class Explain a where
    explain :: a -> String

instance Explain FxTapWarning where
    explain :: FxTapWarning -> String
    explain TitleTrimmed =
        "Title is too long to fit in, only 31 characters are kept."
    explain ArtistTrimmed =
        "Artist is too long to fit in, only 31 characters are kept."
    explain OverlappedHold {..} =
        "The #" ++ show (overlappedHoldIndex + 1) ++
        " note in column #" ++ show (overlappedColumnIndex + 1) ++
        " overlaps with the following " ++ show overlappedNotesCount ++
        " notes."

instance Explain FxTapError where
    explain :: FxTapError -> String
    explain NoteIntervalTooLarge {..} =
        "The #" ++ show (overflowNoteIndex + 1) ++
        " note in column #" ++ show (overflowColumnIndex + 1) ++
        " has an interval of " ++ show overflowedValue ++
        " which is too large."
    explain HoldDurationTooLarge {..} =
        "The #" ++ show (overflowNoteIndex + 1) ++
        " hold note in column #" ++ show (overflowColumnIndex + 1) ++
        " has a duration of " ++ show overflowedValue ++
        " which is too large."

instance Explain FxTapMessage where
    explain :: FxTapMessage -> String
    explain (FxTapWarning warning) = explain warning
    explain (FxTapError error') = explain error'

instance Semigroup FxTapChecker where
    (<>) :: FxTapChecker -> FxTapChecker -> FxTapChecker
    FxTapChecker c1 <> FxTapChecker c2 =
        FxTapChecker $ \fxTap -> c1 fxTap <> c2 fxTap

instance Monoid FxTapChecker where
    mempty :: FxTapChecker
    mempty = FxTapChecker (const mempty)
