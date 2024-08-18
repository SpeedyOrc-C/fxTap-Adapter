module Osu where

data Osu = Osu {
    general :: General,
    editor :: Editor,
    metadata :: Metadata,
    difficulty :: Difficulty,
    events :: Events,
    timingPoints :: TimingPoints,
    colours :: Colours,
    hitObjects :: [OsuHitObject]
}

data General = General {
    audioFilename :: String,
    audioLeadIn :: Integer,
    previewTime :: Integer,
    countdown :: Countdown,
    sampleSet :: SampleSet,
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

data Editor = Editor

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

data Events = Events

data TimingPoints = TimingPoints

data Colours = Colours

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
    volume :: Integer,
    fileName :: String
} deriving Show

data OsuHitObject
    = HitObjectCircle {
        x :: Double, y :: Double,
        time :: Integer,
        hitSound :: HitSound,
        hitSample :: HitSample
    }
    | HitObjectHold {
        x :: Double, y :: Double,
        time :: Integer,
        hitSound :: HitSound,
        endTime :: Integer,
        hitSample :: HitSample
    }
    deriving Show
