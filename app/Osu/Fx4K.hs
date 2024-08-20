module Osu.Fx4K (wFx4K) where

import Data.Binary.Put ( Put, putByteString )
import Data.String (IsString(fromString))

import Osu (Osu)

wHeader :: Put
wHeader = putByteString (fromString "fx4K-beatmap-yay")

wFx4K :: Osu -> Put
wFx4K beatmap = do
    wHeader
