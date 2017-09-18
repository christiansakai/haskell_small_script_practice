#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --install-ghc 
   --package optparse-applicative
   --package wreq
   --package text
-}

import Data.Foldable (forM_)
import Data.Semigroup ((<>))
import Options.Applicative
  ( Parser
  , ParserInfo
  , header
  , progDesc
  , fullDesc
  , info
  , metavar
  , help
  , str
  , argument
  , many
  , helper
  , execParser
  , (<$>)
  , (<*>)
  )
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import Data.Text.Lazy.Encoding ( decodeUtf8 )
import Data.Text.Lazy.IO as TIO ( putStrLn )
import Control.Lens ((^.))

main :: IO ()
main = 
  execParser optsParserInfo >>= \opts ->
    WS.withSession (\session ->
      traverse (WS.get session) (urls opts)
      ) >>= \responses ->
        let bodies = map (\response -> decodeUtf8 $ response ^. W.responseBody) responses
         in forM_ bodies TIO.putStrLn

data Options = Options
  { urls :: [String]
  }

optsParser :: Parser Options
optsParser = Options
  <$> many (
    argument str
      ( metavar "URLS"
      <> help "Urls to request"
      )
    )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  ( fullDesc
  <> progDesc "A bad clone of curl"
  <> header "curl - a bad clone of th real curl"
  )

