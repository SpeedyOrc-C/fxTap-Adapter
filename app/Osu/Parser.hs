module Osu.Parser where

import Data.Map qualified as M
import Data.Bits ( Bits((.&.), shift) )
import Data.Functor ( (<&>), void )
import Text.Parsec (noneOf, char, string, Parsec, digit, (<|>), try, eof)
import Text.Parsec.Combinator (sepBy)
import Control.Applicative ( Alternative(empty, some, many) )

import Osu

b :: Integer -> Integer -> Bool
b bitNumber number = number .&. (2^bitNumber) /= 0

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

pString :: Parser String
pString = many $ noneOf "\r\n\t"

pBool :: Parser Bool
pBool = (True <$ char '1') <|> (False <$ char '0')

pInteger :: Parser Integer
pInteger = do
    sign <- string "-" <|> return ""
    digits <- some digit
    return . read $ sign ++ digits

pDouble :: Parser Double
pDouble = do
    sign <- string "-" <|> return ""
    digitsInt <- some digit
    digitsDec <- ((++) <$> string "." <*> some digit) <|> return ""
    return . read $ sign ++ digitsInt ++ digitsDec

pKv :: String -> Parser a -> Parser a
pKv key pValue = do
    void $ string key
    void . many $ pWhite
    void $ char ':'
    void . many $ pWhite
    value <- pValue
    pLineSeparator

    return value

pSectionTitle :: String -> Parser ()
pSectionTitle title = do
    void $ char '['
    void . many $ pWhite
    void $ string title
    void . many $ pWhite
    void $ char ']'
    pLineSeparator

pKv' :: String -> Parser a -> a -> Parser a
pKv' key pValue defaultValue =
    try (pKv key pValue) <|> return defaultValue

pCountdown :: Parser Countdown
pCountdown = do
    c <- digit
    case c of
        '0' -> return CountdownNo
        '1' -> return CountdownNormal
        '2' -> return CountdownHalf
        '3' -> return CountdownDouble
        _ -> empty

pSampleSet :: Parser (Maybe SampleSet)
pSampleSet = do
        Nothing <$ try (string "None")
    <|> Just SampleSetNormal <$ try (string "Normal")
    <|> Just SampleSetDrum <$ string "Drum"
    <|> Just SampleSetSoft <$ string "Soft"

pMode :: Parser Mode
pMode = do
    c <- digit
    case c of
        '0' -> return ModeOsu
        '1' -> return ModeTaiko
        '2' -> return ModeCatch
        '3' -> return ModeMania
        _ -> empty

pOverlayPosition :: Parser OverlayPosition
pOverlayPosition =
        OverlayPositionNoChange <$ string "NoChange"
    <|> OverlayPositionBelow <$ string "Below"
    <|> OverlayPositionAbove <$ string "Above"

pGeneral :: Parser General
pGeneral = do
    pSectionTitle "General"
    General
        <$> pKv "AudioFilename" pString
        <*> pKv' "AudioLeadIn" pInteger 0
        <*> pKv' "PreviewTime" pInteger (-1)
        <*> pKv' "Countdown" pCountdown CountdownNormal
        <*> pKv' "SampleSet" pSampleSet (Just SampleSetNormal)
        <*> pKv' "StackLeniency" pDouble 0.7
        <*> pKv' "Mode" pMode ModeOsu
        <*> pKv' "LetterboxInBreaks" pBool False
        <*> pKv' "UseSkinSprites" pBool False
        <*> pKv' "OverlayPosition" pOverlayPosition OverlayPositionNoChange
        <*> pKv' "SkinPreference" pString ""
        <*> pKv' "EpilepsyWarning" pBool False
        <*> pKv' "CountdownOffset" pInteger 0
        <*> pKv' "SpecialStyle" pBool False
        <*> pKv' "WidescreenStoryboard" pBool False
        <*> pKv' "SamplesMatchPlaybackRate" pBool False

pEditor :: Parser Editor
pEditor = do
    pSectionTitle "Editor"
    Editor
        <$> pKv' "Bookmarks" (pInteger `sepBy` char ',') []
        <*> pKv "DistanceSpacing" pDouble
        <*> pKv "BeatDivisor" pInteger
        <*> pKv "GridSize" pInteger
        <*> pKv "TimelineZoom" pDouble

pType :: Parser Type
pType = pInteger <&>
    \i -> Type (b 0 i) (b 1 i) (b 2 i) (b 3 i)
        (shift i (-3) .&. (1 + 2 + 4)) (b 7 i)

pHitSound :: Parser HitSound
pHitSound = pInteger <&> \i -> HitSound (b 0 i) (b 1 i) (b 2 i) (b 3 i)

pMaybeSampleSet :: Parser (Maybe SampleSet)
pMaybeSampleSet = do
        Nothing <$ char '0'
    <|> Just SampleSetNormal <$ char '1'
    <|> Just SampleSetSoft <$ char '2'
    <|> Just SampleSetDrum <$ char '3'

pHitSample :: Parser HitSample
pHitSample = HitSample
    <$> pMaybeSampleSet <* char ':'
    <*> pMaybeSampleSet <* char ':'
    <*> pInteger <* char ':'
    <*> pInteger <* char ':'
    <*> pString

pHitSampleOptionalHitCircle :: Parser HitSample
pHitSampleOptionalHitCircle =
    (char ',' *> pHitSample) <|> return (HitSample Nothing Nothing 0 0 "")

pHitSampleOptionalHold :: Parser HitSample
pHitSampleOptionalHold =
    (char ':' *> pHitSample) <|> return (HitSample Nothing Nothing 0 0 "")

pHitObject :: Parser OsuHitObject
pHitObject = do
    x <- pDouble <* char ','
    y <- pDouble <* char ','
    time <- pInteger <* char ','
    type' <- pType <* char ','
    hitSound <- pHitSound

    if isHitCircle type' then
        HitObjectCircle x y time hitSound
            <$> pHitSampleOptionalHitCircle

    else if isHold type' then do
        void $ char ','
        endTime <- pInteger
        HitObjectHold x y time hitSound endTime
            <$> pHitSampleOptionalHold

    else
        error "Unexpected hit object type."

pHitObjects :: Parser [OsuHitObject]
pHitObjects = do
    pSectionTitle "HitObjects"
    some (pHitObject <* pLineSeparator)

pDifficulty :: Parser Difficulty
pDifficulty = do
    pSectionTitle "Difficulty"
    Difficulty
        <$> pKv "HPDrainRate" pDouble
        <*> pKv "CircleSize" pDouble
        <*> pKv "OverallDifficulty" pDouble
        <*> pKv "ApproachRate" pDouble
        <*> pKv "SliderMultiplier" pDouble
        <*> pKv "SliderTickRate" pDouble

pTag :: Parser String
pTag = some $ noneOf " \t\r\n"

pTags :: Parser [String]
pTags = pTag `sepBy` some pWhite

pMetadata :: Parser Metadata
pMetadata = do
    pSectionTitle "Metadata"
    Metadata
        <$> pKv "Title" pString
        <*> pKv "TitleUnicode" pString
        <*> pKv "Artist" pString
        <*> pKv "ArtistUnicode" pString
        <*> pKv "Creator" pString
        <*> pKv "Version" pString
        <*> pKv "Source" pString
        <*> pKv "Tags" pTags
        <*> pKv "BeatmapID" pString
        <*> pKv "BeatmapSetID" pString

pEffects :: Parser Effects
pEffects = pInteger <&> \i -> Effects (b 0 i) (b 3 i)

pSampleSetTimingPoint :: Parser (Maybe SampleSet)
pSampleSetTimingPoint =
        Nothing <$ char '0'
    <|> Just SampleSetNormal <$ char '1'
    <|> Just SampleSetSoft <$ char '2'
    <|> Just SampleSetDrum <$ char '3'

pTimingPoint :: Parser TimingPoint
pTimingPoint = TimingPoint
    <$> pDouble <* char ','
    <*> pDouble <* char ','
    <*> pInteger <* char ','
    <*> pSampleSetTimingPoint <* char ','
    <*> pInteger <* char ','
    <*> pInteger <* char ','
    <*> pBool <* char ','
    <*> pEffects

pTimingPoints :: Parser [TimingPoint]
pTimingPoints = do
    pSectionTitle "TimingPoints"
    some $ pTimingPoint <* pLineSeparator

pHeader :: Parser ()
pHeader = do
    void $ string "osu file format v14"
    pLineSeparator

pStringInList :: Parser String
pStringInList = do
    hasQuote <- True <$ char '\"' <|> return False
    if hasQuote then
        many (noneOf "\"\r\n") <* char '\"'
    else do
        many (noneOf ",\r\n")

pOptionalOffset :: Parser (Integer, Integer)
pOptionalOffset =
    ((,) <$> (char ',' *> pInteger) <*> (char ',' *> pInteger))
    <|> return (0, 0)

pEvent :: Parser Event
pEvent = pEventVideo <|> do
    type' <- pInteger <* char ','
    case type' of
        0 -> string "0," *>
            (Background
            <$> pStringInList
            <*> pOptionalOffset)
        1 ->
            Video
            <$> pInteger <* char ','
            <*> pStringInList
            <*> pOptionalOffset
        2 ->
            Break
            <$> pInteger <* char ','
            <*> pInteger
        _ -> error "Not implemented event type."

pEventVideo :: Parser Event
pEventVideo = do
    void $ string "Video,"
    Video
        <$> pInteger <* char ','
        <*> pStringInList
        <*> pOptionalOffset

pEvents :: Parser [Event]
pEvents = do
    pSectionTitle "Events"
    some $ pEvent <* pLineSeparator

pColour :: Parser Colour
pColour = Colour
    <$> pInteger <* char ','
    <*> pInteger <* char ','
    <*> pInteger

pComboColour :: Parser (Integer, Colour)
pComboColour = do
    n <- string "Combo" *> pInteger
    pWhite
    void $ char ':'
    pWhite
    colour <- pColour
    pLineSeparator

    return (n, colour)

pColours :: Parser Colours
pColours = do
    pSectionTitle "Colours"
    Colours
        <$> (M.fromList <$> some pComboColour)
        <*> pKv' "SliderTrackOverride" (Just <$> pColour) Nothing
        <*> pKv' "SliderBorder" (Just <$> pColour) Nothing

pOsu :: Parser Osu
pOsu = do
    pHeader
    Osu
        <$> pGeneral
        <*> pEditor
        <*> pMetadata
        <*> pDifficulty
        <*> pEvents
        <*> pTimingPoints
        <*> (try (Just <$> pColours) <|> return Nothing)
        <*> pHitObjects
        <* eof
