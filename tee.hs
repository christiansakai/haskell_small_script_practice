#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --install-ghc 
   --package optparse-applicative
-}

import Debug.Trace (trace)
import Data.Semigroup ((<>))
import Options.Applicative
  ( execParser
  , ParserInfo
  , Parser
  , some
  , argument
  , str
  , metavar
  , help
  , switch
  , short
  , long
  , info
  , helper
  , fullDesc
  , progDesc
  , header
  , (<$>)
  , (<*>)
  )
import System.IO
  ( IOMode 
    ( AppendMode
     , WriteMode
     )
  , openFile
  , hPutStrLn
  , hClose
  )
import Data.Char (isSpace)

trace' a = trace ("trace => " ++ show a) a

main :: IO ()
main =
  execParser optsParserInfo >>= \opts ->
    let filemode = if append opts
                    then AppendMode
                    else WriteMode
     in mapM (`openFile` filemode) (filenames opts) >>= \fileHandles ->
          getContents >>= \input ->
            let trimmedInput = trimEof input
             in mapM (`hPutStrLn` input) fileHandles >>
                  mapM_ hClose fileHandles

data Options = Options
  { filenames :: [String]
  , append :: Bool
  } deriving Show

optsParser :: Parser Options
optsParser = Options
  <$> some 
    ( argument str
      ( metavar "FILENAMES"
      <> help "Output files"
      )
    )
  <*> switch
    ( long "append"
    <> short 'a'
    <> help "Append to output file rather than overwrite"
    )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  ( fullDesc
  <> progDesc "A bad clone of head, only work when sending signal ^D for termination"
  <> header "tee - a bad clone of the real tee"
  )

trimEof :: String -> String
trimEof = f . f
  where f = reverse . dropWhile (== '\n')
