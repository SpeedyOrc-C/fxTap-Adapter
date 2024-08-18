module Osu.Parser where

import Data.Bits
import Data.Functor
import Control.Applicative ( Alternative(empty, some, many) )
import Text.Parsec (noneOf, char, string, Parsec, letter, digit, (<|>), try)
import Text.Parsec.Combinator (sepBy)

import Osu


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

pSampleSet :: Parser SampleSet
pSampleSet = do
        SampleSetNormal <$ string "Normal"
    <|> SampleSetDrum <$ string "Drum"
    <|> SampleSetSoft <$ string "Soft"

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
    General <$>
            pKv "AudioFilename" pString
        <*> pKv' "AudioLeadIn" pInteger 0
        <*> pKv' "PreviewTime" pInteger (-1)
        <*> pKv' "Countdown" pCountdown CountdownNormal
        <*> pKv' "SampleSet" pSampleSet SampleSetNormal
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

pType :: Parser Type
pType = pInteger <&>
    \i -> Type
        (i .&. 1 /= 0)
        (i .&. 2 /= 0)
        (i .&. 4 /= 0)
        (i .&. 8 /= 0)
        (shift i (-3) .&. (1 + 2 + 4))
        (i .&. 128 /= 0)

pHitSound :: Parser HitSound
pHitSound = pInteger <&>
    \i -> HitSound
        (i .&. 1 /= 0)
        (i .&. 2 /= 0)
        (i .&. 4 /= 0)
        (i .&. 8 /= 0)

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

pOptionalHitSample :: Parser HitSample
pOptionalHitSample =
    (char ',' *> pHitSample) <|> return (HitSample Nothing Nothing 0 0 "")

pHitObject :: Parser OsuHitObject
pHitObject = do
    x <- pDouble
    void $ char ','
    y <- pDouble
    void $ char ','
    time <- pInteger
    void $ char ','
    type' <- pType
    void $ char ','
    hitSound <- pHitSound

    if isHitCircle type' then
        HitObjectCircle x y time hitSound
            <$> pOptionalHitSample

    else if isHold type' then do
        void $ char ','
        endTime <- pInteger
        HitObjectHold x y time hitSound endTime
            <$> pOptionalHitSample

    else
        error "Unexpected hit object type."

pHitObjects :: Parser [OsuHitObject]
pHitObjects = do
    pSectionTitle "HitObjects"
    some (pHitObject <* pLineSeparator)

pDifficulty :: Parser Difficulty
pDifficulty = do
    pSectionTitle "Difficulty"
    Difficulty <$>
            pKv "HPDrainRate" pDouble
        <*> pKv "CircleSize" pDouble
        <*> pKv "OverallDifficulty" pDouble
        <*> pKv "ApproachRate" pDouble
        <*> pKv "SliderMultiplier" pDouble
        <*> pKv "SliderTickRate" pDouble

pTag :: Parser String
pTag = some $ noneOf " \t\r\n"

pMetadata :: Parser Metadata
pMetadata = do
    pSectionTitle "Metadata"
    title <- pString
    pLineSeparator
    titleUnicode <- pString
    pLineSeparator
    artist <- pString
    pLineSeparator
    artistUnicode <- pString
    pLineSeparator
    creator <- pString
    pLineSeparator
    version <- pString
    pLineSeparator
    source <- pString
    pLineSeparator
    tags <- pTag `sepBy` pWhite
    pLineSeparator
    beatmapId <- pString
    pLineSeparator
    beatmapSetId <- pString
    pLineSeparator
    return $ Metadata
        title titleUnicode artist artistUnicode creator
        version source tags beatmapId beatmapSetId
