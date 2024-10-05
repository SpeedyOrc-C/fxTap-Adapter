{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.Beatmap.Malody where

import Data.Aeson ( (.:), withArray, withObject, FromJSON(parseJSON), Value )
import Data.Aeson.Types (Parser)
import Data.Ratio ((%))
import Data.Vector qualified as V
import Data.Maybe (catMaybes)
import Data.Function ((&), on)
import Data.List (groupBy, sortOn)
import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad.State (State, MonadState (get, put), evalState)
import Control.Arrow ((>>>))

import Data.Beatmap.IR qualified as IR
import Data.Beatmap.IR (INoteCompatible (..), INote)
import Data.Beatmap.FxTap (FxTapCompatible (..))

data Song = Song {
    title :: String,
    artist :: String,
    songId :: Integer
} deriving Show

data MalodyBeat = MalodyBeat Integer Integer Integer

data BpmChange = BpmChange {
    changeBeat :: Rational,
    bpm :: Double
} deriving Show

data Event
    = Tap {
        startBeat :: Rational,
        column :: Integer,
        style :: Integer
    }
    | Hold {
        startBeat :: Rational,
        endBeat :: Rational,
        column :: Integer,
        style :: Integer,
        hits :: Integer
    }
    | Sound {
        startBeat :: Rational,
        sound :: FilePath,
        volume :: Integer,
        offset :: Integer,
        soundType :: Integer
    }
    deriving Show

data Metadata = Metadata {
    creator :: String,
    background :: FilePath,
    version :: String,
    beatmapId :: Integer,
    mode :: Integer,
    publishTime :: Integer,
    song :: Song
} deriving Show

data Malody = Malody {
    metadata :: Metadata,
    bpmChanges :: [BpmChange],
    events :: [Event]
} deriving Show

instance FromJSON MalodyBeat where
    parseJSON :: Value -> Parser MalodyBeat
    parseJSON = withArray "Beat" $ \xs -> do
        if V.length xs /= 3 then
            empty
        else
            MalodyBeat
            <$> parseJSON (xs V.! 0)
            <*> parseJSON (xs V.! 1)
            <*> parseJSON (xs V.! 2)

malodyBeatToRational :: MalodyBeat -> Rational
malodyBeatToRational (MalodyBeat a b c) = fromInteger a + (b % c)

instance FromJSON Event where
    parseJSON :: Value -> Parser Event
    parseJSON = withObject "Note" $ \o ->
        -- WARNING: Meaning of `style` unknown!
        Hold
            <$> fmap malodyBeatToRational (o .: "beat")
            <*> fmap malodyBeatToRational (o .: "endbeat")
            <*> o .: "column"
            <*> (o .: "style" <|> return 0)
            <*> (o .: "hits" <|> return 1)
        <|>
        Sound
            <$> fmap malodyBeatToRational (o .: "beat")
            <*> o .: "sound"
            <*> (o .: "vol" <|> return 100)
            <*> o .: "offset"
            <*> o .: "type"
        <|>
        Tap
            <$> fmap malodyBeatToRational (o .: "beat")
            <*> o .: "column"
            <*> (o .: "style" <|> return 0)

instance FromJSON Song where
    parseJSON :: Value -> Parser Song
    parseJSON = withObject "Song" $ \o ->
        Song
        <$> o .: "title"
        <*> o .: "artist"
        <*> o .: "id"

instance FromJSON BpmChange where
    parseJSON :: Value -> Parser BpmChange
    parseJSON = withObject "BpmChange" $ \o ->
        BpmChange
        <$> fmap malodyBeatToRational (o .: "beat")
        <*> o .: "bpm"

instance FromJSON Metadata where
    parseJSON :: Value -> Parser Metadata
    parseJSON = withObject "Metadata" $ \o ->
        Metadata
        <$> o .: "creator"
        <*> o .: "background"
        <*> o .: "version"
        <*> o .: "id"
        <*> o .: "mode"
        <*> o .: "time"
        <*> o .: "song"

instance FromJSON Malody where
    parseJSON :: Value -> Parser Malody
    parseJSON = withObject "Malody" $ \o ->
        Malody
        <$> o .: "meta"
        <*> o .: "time"
        <*> o .: "note"

instance INoteCompatible Malody where
    getINoteColumns :: Malody -> [[INote]]
    getINoteColumns Malody { bpmChanges, events } = events
        & filter isNote
        & sortOn column
        & groupBy ((==) `on` column)
        & map
            (   sortOn startBeat
            >>> getINoteColumn)

        where

        isNote :: Event -> Bool
        isNote Tap {} = True
        isNote Hold {} = True
        isNote _ = False

        getINoteColumn :: [Event] -> [INote]
        getINoteColumn notes =
            catMaybes $
                evalState
                    (traverse walkerConverter notes)
                    (zip bpmChangesIntervals bpmChanges)

        bpmChangesIntervals :: [Double]
        bpmChangesIntervals =
            -- There is always a initial BPM at the beginning (0 sec)
            (0:) $
            -- Accumulate the intervals
            scanl1 (+) $
                -- Put adjacent changes into pairs
                zipWith interval bpmChanges (tail bpmChanges)
                where
                    -- Calculate the interval (ms) between two BPM change points.
                    interval (BpmChange t bpm) (BpmChange t' _) =
                        fromRational (t' - t) / bpm * 60 * 1000

        -- Each BPM change is attached by its starting time (Double) in state.
        walkerConverter :: Event -> State [(Double, BpmChange)] (Maybe INote)
        walkerConverter note = get >>= \case
            [] -> error "BPM lost, cannot determine note time."

            cs@[(t, BpmChange b bpm)] -> do
                let msFromBeat x = fromRational x / bpm * 60 * 1000
                let startTime = t + msFromBeat (startBeat note - b)

                put cs
                return $
                    case note of
                        Tap {} -> Just $ IR.ITap (round startTime)

                        Hold { endBeat } -> do
                            let endTime = t + msFromBeat (endBeat - b)
                            Just $ IR.IHold (round startTime) (round endTime)

                        _ -> Nothing

            (c@(t0, BpmChange b0 bpm) : c'@(t1, _) : cs) -> do
                let startTime = t0 + fromRational (startBeat note - b0) / bpm * 60 * 1000
                put $
                    if startTime > t1 then
                        -- If this note is later than this BPM interval,
                        -- forward to the next BPM interval.
                        c':cs
                    else
                        -- Otherwise use the current BPM.
                        [c]
                walkerConverter note

instance FxTapCompatible Malody where
    getTitle :: Malody -> String
    getTitle = title . song . metadata

    getArtist :: Malody -> String
    getArtist = artist . song . metadata

    getOverallDifficulty :: Malody -> Double
    getOverallDifficulty = const 6
