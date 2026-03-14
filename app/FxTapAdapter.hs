{-# LANGUAGE LambdaCase #-}

module FxTapAdapter where

import Control.Applicative (Alternative ((<|>)), optional)
import GHC.ByteOrder (ByteOrder (BigEndian, LittleEndian))
import Options.Applicative (Parser, completeWith, eitherReader, flag', fullDesc, help, helper, long, metavar, option, short, strOption, (<**>))
import Options.Applicative.Builder (info)
import Options.Applicative.Extra (execParser)

data FxTapArgs
   = FxTapMain
      { outputType :: OutputType
      , inputPath :: FilePath
      , outputPath :: Maybe FilePath
      }
   | FxTapVersion

data OutputType = OutputBinary ByteOrder | OutputC String

getFxtaArgs :: IO FxTapArgs
getFxtaArgs = execParser (info (parseFxtaArgs <**> helper) fullDesc)

parseFxtaArgs :: Parser FxTapArgs
parseFxtaArgs =
   pMain <|> pVersion
  where
   pOutputBinary =
      OutputBinary
         <$> pEndian
            ( long "bin"
               <> short 'b'
               <> metavar "big|little"
               <> completeWith ["big", "casio", "little", "windows", "macos", "linux"]
               <> help "Generate a binary file with specified endianness."
            )
     where
      pEndian = option . eitherReader $ \case
         "b" -> Right BigEndian
         "big" -> Right BigEndian
         "casio" -> Right BigEndian
         "l" -> Right LittleEndian
         "little" -> Right LittleEndian
         "windows" -> Right LittleEndian
         "macos" -> Right LittleEndian
         "linux" -> Right LittleEndian
         _ -> Left "Invalid endianness"

   pOutputCHeader =
      fmap OutputC . strOption $
         ( long "c"
            <> short 'c'
            <> metavar "IDENTIFIER"
            <> help "Generate a C and a header file with the beatmap hardcoded, identified by a specified name"
         )

   pOutputType = pOutputBinary <|> pOutputCHeader

   pInputPath =
      strOption
         ( long "input"
            <> short 'i'
            <> metavar "PATH"
            <> help "Path to the beatmap to be converted"
         )

   pOutputPath =
      optional . strOption $
         ( long "output"
            <> short 'o'
            <> metavar "PATH"
            <> help "Output path"
         )

   pVersion =
      flag'
         FxTapVersion
         ( long "version"
            <> short 'v'
            <> help "Show version"
         )

   pMain :: Parser FxTapArgs
   pMain = FxTapMain <$> pOutputType <*> pInputPath <*> pOutputPath
