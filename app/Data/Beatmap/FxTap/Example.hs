module Data.Beatmap.FxTap.Example where

import Data.Beatmap.FxTap

data FxTapExample = ExampleHold

exampleHold :: FxTap
exampleHold =
   FxTap
      { title = "Example (hold)"
      , artist = "fxTap Adapter"
      , overallDifficulty = 3
      , noteColumns =
         [ Hold 0 1000
            : replicate 127 (Hold 2000 1000)
         , [Tap 0]
         , [Tap 0]
         , [Tap 0]
         ]
      }
