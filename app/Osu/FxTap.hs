{-# LANGUAGE LambdaCase #-}
module Osu.FxTap (wFxTap) where

import Data.Binary.Put ( Put, putStringUtf8)
import Data.Function ( (&) )

import Osu (Osu (metadata, hitObjects), Metadata (title, artist), OsuHitObject (..))
import Data.Foldable (for_)

str32 :: String -> String
str32 string =
    if stringLength <= 32 then
        string ++ replicate (32 - stringLength) ' '
    else
        take 32 string
    where
        stringLength = length string

wFxTap :: Osu -> Put
wFxTap beatmap = do
    putStringUtf8 "fx4K-beatmap-yay"

    putStringUtf8 $ beatmap & metadata & title & str32
    putStringUtf8 $ beatmap & metadata & artist & str32

    for_ (beatmap & hitObjects) $ \case

        HitObjectCircle x _ time _ _ -> do
            undefined

        HitObjectHold x _ time _ endTime _ -> do
            undefined
