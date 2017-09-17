#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --install-ghc 
   --package optparse-applicative
-}

import Debug.Trace
import Data.Semigroup ((<>))
import Options.Applicative 
  ( Parser
  , ParserInfo
  , execParser
  , argument
  , strOption
  , long
  , metavar
  , help
  , showDefault
  , value
  , auto
  , short
  , option
  , str
  , helper
  , info
  , header
  , fullDesc
  , progDesc
  , (<$>)
  , (<*>)
  )

trace' a = trace ("trace => " ++ show a) a

main :: IO ()
main = 
  execParser optsParserInfo >>= \opts ->
    readFile (filename opts) >>= \contents ->
      putStrLn $ unlines $ take (lineCount opts) (lines contents)

data Options = Options 
  { filename :: String
  , lineCount :: Int
  }
    
optsParser :: Parser Options
optsParser = Options
  <$> argument str
    ( metavar "FILENAME"
    <> help "Input filename"
    )
  <*> option auto
    ( short 'n'
    <> long "numlines"
    <> metavar "NUM"
    <> help "Number of lines to read"
    <> showDefault
    <> value 10
    )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  ( fullDesc
  <> progDesc "A bad clone of head"
  <> header "head - a bad clone of the real head"
  )

