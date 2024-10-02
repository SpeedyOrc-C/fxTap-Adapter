module FxTap where

class FxTapCompatible beatmap where
    toFxTap :: beatmap -> FxTap

data FxTap = FxTap {
    title :: String,
    artist :: String,
    overallDifficulty :: Double,
    -- 1st dimension : columns
    -- 2nd dimension : notes
    notesColumns :: [[Note]]
} deriving Show

data Note
    = Tap { accumulatedStartTime :: Integer }
    | Hold { accumulatedStartTime :: Integer, duration :: Integer }
    deriving Show
