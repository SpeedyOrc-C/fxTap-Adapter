module Osu where
import qualified Data.Map as M

data Osu = Osu {
    general :: General,
    editor :: Editor,
    metadata :: Metadata,
    difficulty :: Difficulty,
    events :: [Event],
    timingPoints :: [TimingPoint],
    colours :: Colours,
    hitObjects :: [OsuHitObject]
}

data General = General {
    audioFilename :: String,
    audioLeadIn :: Integer,
    previewTime :: Integer,
    countdown :: Countdown,
    sampleSetGeneral :: SampleSet,
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
        xOffset :: Integer,
        yOffset :: Integer
    }
    | Video {
        startTimeEvent :: Integer,
        fileNameVideo :: String,
        xOffset :: Integer,
        yOffset :: Integer
    }
    | Break {
        startTimeEvent :: Integer,
        endTimeEvent :: Integer
    }
    -- TODO: Storyboard
    deriving Show

data Effects = Effects {
    kiaiTime :: Bool,
    omitFirstBarline :: Bool
} deriving Show

data TimingPoint = TimingPoint {
    startTime :: Integer,
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
    sliderTrackOverride :: Colour,
    sliderBorder :: Colour
}

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
