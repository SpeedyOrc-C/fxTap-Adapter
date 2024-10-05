module Data.Beatmap.FxTap where

import Control.Monad.State (MonadState(state), State, evalState)
import Data.Beatmap.IR

class INoteCompatible beatmap => FxTapCompatible beatmap where
    getTitle :: beatmap -> String
    getArtist :: beatmap -> String
    getOverallDifficulty :: beatmap -> Double

    toFxTap :: beatmap -> FxTap
    toFxTap beatmap = FxTap {
        title = getTitle beatmap,
        artist = getArtist beatmap,
        overallDifficulty = getOverallDifficulty beatmap,
        noteColumns =
            (\x -> evalState (traverse accumulateAbsoluteNotes x) 0)
                <$> getINoteColumns beatmap
    }

data FxTap = FxTap {
    title :: String,
    artist :: String,
    overallDifficulty :: Double,
    -- 1st dimension : columns
    -- 2nd dimension : notes
    noteColumns :: [[Note]]
} deriving Show

data Note
    = Tap { accumulatedStartTime :: Integer }
    | Hold { accumulatedStartTime :: Integer, duration :: Integer }
    deriving Show

accumulateAbsoluteNotes :: INote -> State Integer Note
accumulateAbsoluteNotes (ITap t) = state $ \currentTime ->
    (Tap (t - currentTime), t)
accumulateAbsoluteNotes (IHold t t') = state $ \currentTime ->
    (Hold (t - currentTime) (t' - t), t)
