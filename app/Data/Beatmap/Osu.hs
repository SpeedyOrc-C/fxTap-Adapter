{-# LANGUAGE LambdaCase #-}

module Data.Beatmap.Osu where

import Data.Map qualified as M
import Data.List (groupBy, sortOn)
import Data.Function (on, (&))

import Data.Beatmap.IR qualified as IR
import Data.Beatmap.IR ( INote, INoteCompatible (..) )
import Data.Beatmap.FxTap (FxTapCompatible (..))

data Osu = Osu {
    general :: General,
    editor :: Editor,
    metadata :: Metadata,
    difficulty :: Difficulty,
    events :: [Event],
    timingPoints :: [TimingPoint],
    colours :: Maybe Colours,
    hitObjects :: [OsuHitObject]
} deriving Show

data General = General {
    audioFilename :: String,
    audioLeadIn :: Integer,
    previewTime :: Integer,
    countdown :: Countdown,
    sampleSetGeneral :: Maybe SampleSet,
    stackLeniency :: Double,
    mode :: Mode,
    letterboxInBreaks :: Bool,
    useSkinSprites :: Bool,
    overlayPosition :: OverlayPosition,
    skinPreference :: String,
    epilepsyWarning :: Bool,
    countdownOffset :: Integer,
    specialStyle :: Bool,
    widescreenStoryboard :: Bool,
    samplesMatchPlaybackRate :: Bool
} deriving Show

data Editor = Editor {
    bookmarks :: [Integer],
    distanceSpacing :: Double,
    beatDivisor :: Integer,
    gridSize :: Integer,
    timelineZoom :: Double
} deriving Show

data Countdown =
    CountdownNo | CountdownNormal | CountdownHalf | CountdownDouble
    deriving Show
data SampleSet =
    SampleSetNormal | SampleSetSoft | SampleSetDrum
    deriving Show
data Mode =
    ModeOsu | ModeTaiko | ModeCatch | ModeMania
    deriving Show
data OverlayPosition =
    OverlayPositionNoChange | OverlayPositionBelow | OverlayPositionAbove
    deriving Show

data Metadata = Metadata {
    title :: String,
    titleUnicode :: String,
    artist :: String,
    artistUnicode :: String,
    creator :: String,
    version :: String,
    source :: String,
    tags :: [String],
    beatmapId :: String,
    beatmapSetId :: String
} deriving Show

data Difficulty = Difficulty {
    hpDrainRate :: Double,
    circleSize :: Double,
    overallDifficulty :: Double,
    approachRate :: Double,
    sliderMultiplier :: Double,
    sliderTickRate :: Double
} deriving Show

data Event
    = Background {
        fileNameEvent :: String,
        offset :: (Integer, Integer)
    }
    | Video {
        startTimeEvent :: Integer,
        fileNameEvent :: String,
        offset :: (Integer, Integer)
    }
    | Break {
        startTimeEvent :: Integer,
        endTimeEvent :: Integer
    }
    -- TODO: Storyboard
    | AudioSample {
        startTimeEvent :: Integer,
        layerNumber :: Integer,
        fileNameEvent :: String,
        volume :: Integer
    }
    deriving Show

data Effects = Effects {
    kiaiTime :: Bool,
    omitFirstBarline :: Bool
} deriving Show

data TimingPoint = TimingPoint {
    startTime :: Double,
    beatLength :: Double,
    meter :: Integer,
    sampleSetTimingPoint :: Maybe SampleSet,
    sampleIndex :: Integer,
    volumeTimingPoint :: Integer,
    uninherited :: Bool,
    effects :: Effects
} deriving Show

data Colour = Colour Integer Integer Integer
    deriving Show

data Colours = Colours {
    combo :: Integer `M.Map` Colour,
    sliderTrackOverride :: Maybe Colour,
    sliderBorder :: Maybe Colour
} deriving Show

data Type = Type {
    isHitCircle :: Bool,
    isSlider :: Bool,
    newComboStart :: Bool,
    isSpinner :: Bool,
    comboColourSkipNumber :: Integer,
    isHold :: Bool
} deriving Show

data HitSound = HitSound {
    hitSoundNormal :: Bool,
    hitSoundWhistle :: Bool,
    hitSoundFinish :: Bool,
    hitSoundClap :: Bool
} deriving Show

data HitSample = HitSample {
    normalSet :: Maybe SampleSet,
    additionSet :: Maybe SampleSet,
    index :: Integer,
    volumeHitSample :: Integer,
    fileNameHitSample :: String
} deriving Show

data OsuHitObject
    = HitObjectCircle {
        x :: Double, y :: Double,
        timeHitObject :: Integer,
        hitSound :: HitSound,
        hitSample :: HitSample
    }
    | HitObjectHold {
        x :: Double, y :: Double,
        timeHitObject :: Integer,
        hitSound :: HitSound,
        endTime :: Integer,
        hitSample :: HitSample
    }
    deriving Show

instance INoteCompatible Osu where
    getINoteColumns :: Osu -> [[INote]]
    getINoteColumns (Osu { hitObjects, difficulty }) =
        hitObjects
        -- Calculate each note's column index, this is because each column
        -- accepts notes that have an x-coord locates inside a specific interval.
        & map (\hitObject -> (osuXToColumn (x hitObject), hitObject))
        -- Sort by column index
        & sortOn fst
        -- Group by column index
        & groupBy ((==) `on` fst)
        -- Drop the redundant column index with `snd` and convert them to IR
        & map (map (convert . snd))

        where
            osuXToColumn :: Double -> Integer
            osuXToColumn x = floor $ x * circleSize difficulty / 512.0

            convert = \case
                HitObjectCircle { timeHitObject } ->
                    IR.ITap timeHitObject
                HitObjectHold { timeHitObject, endTime } ->
                    IR.IHold timeHitObject endTime

instance FxTapCompatible Osu where
    getTitle :: Osu -> String
    getTitle = title . metadata

    getArtist :: Osu -> String
    getArtist = artist . metadata

    getOverallDifficulty :: Osu -> Double
    getOverallDifficulty = overallDifficulty . difficulty

