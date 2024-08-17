module Main where

import Data.Map qualified as M

import Control.Monad
import Control.Applicative

import Text.Parsec (noneOf, char, string, Parsec, letter, digit)

data Osu = Osu {
    osuGeneral :: OsuGeneral,
    osuEditor :: OsuEditor,
    osuMetadata :: OsuMetadata,
    osuDifficulty :: OsuDifficulty,
    osuEvents :: OsuEvents,
    osuTimingPoints :: OsuTimingPoints,
    osuColours :: OsuColours,
    osuHitObjects :: OsuHitObjects
}

data OsuGeneral = OsuGeneral {
    generalAudioFilename :: String,
    generalAudioLeadIn :: Integer,
    generalPreviewTime :: Integer,
    generalCountdown :: GeneralCountdown,
    generalSampleSet :: GeneralSampleSet,
    generalStackLeniency :: Double,
    generalMode :: GeneralMode,
    generalLetterboxInBreaks :: Bool,
    generalUseSkinSprites :: Bool,
    generalOverlayPosition :: GeneralOverlayPosition,
    generalSkinPreference :: String,
    generalEpilepsyWarning :: Bool,
    generalCountdownOffset :: Integer,
    generalSpecialStyle :: Bool,
    generalWidescreenStoryboard :: Bool,
    generalSamplesMatchPlaybackRate :: Bool
}

data OsuEditor = OsuEditor

data GeneralCountdown = CountdownNo | CountdownNormal | CountdownHalf | CountdownDouble
data GeneralSampleSet = SampleSetNormal | SampleSetSoft | SampleSetDrum
data GeneralMode = ModeOsu | ModeTaiko | ModeCatch | ModeMania
data GeneralOverlayPosition = OverlayPositionNoChange | OverlayPositionBelow | OverlayPositionAbove

data OsuMetadata = Metadata

data OsuDifficulty = Difficulty

data OsuEvents = Events

data OsuTimingPoints = TimingPoints

data OsuColours = Colours

data OsuHitObjects = HitObjects

type Parser a = Parsec String () a

pLineBreak :: Parser ()
pLineBreak = void $ string "\n" <|> string "\r\n"

pWhite :: Parser ()
pWhite = void (char ' ' <|> char '\t') <|> pComment

pComment :: Parser ()
pComment = do
    void $ string "//"
    void . many $ noneOf "\n\r"

pLineSeparator :: Parser ()
pLineSeparator = void $ do
    void $ many pWhite
    pLineBreak
    void $ many (pLineBreak <|> pWhite)

pKey :: Parser String
pKey = some $ letter <|> digit

pKvPair :: String -> Parser a -> Parser a
pKvPair key pValue = do
    void $ string key
    void . many $ pWhite
    void $ char ':'
    void . many $ pWhite
    pValue

pSectionTitle :: String -> Parser ()
pSectionTitle title = do
    void $ char '['
    void . many $ pWhite
    void $ string title
    void . many $ pWhite
    void $ char ']'

pKvPairOptional :: String -> Parser a -> a -> Parser a
pKvPairOptional key pValue defaultValue =
    pKvPair key pValue <|> return defaultValue

main :: IO ()
main = putStrLn "Hello, Haskell!"
