module Data.Beatmap.IR where

data INote
    = ITap { startTime :: Integer }
    | IHold { startTime :: Integer, endTime :: Integer }
    deriving Show

class INoteCompatible beatmap where
    getINoteColumns :: beatmap -> [[INote]]
