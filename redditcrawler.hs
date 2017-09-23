#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --install-ghc 
   --package optparse-applicative
   --package aeson
   --package cassava
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup ((<>))
import Options.Applicative
  ( Parser
  , ParserInfo
  , execParser
  , argument
  , fullDesc
  , header
  , help
  , helper
  , info
  , metavar
  , progDesc
  , str
  , (<*>)
  )
import Data.Text (Text)
import qualified Network.Wreq as W
import qualified Network.Wreq.Session as WS
import qualified Data.Aeson as Aeson
import Data.Aeson 
  ( FromJSON
  , Object
  , parseJSON
  , (.:)
  )
import Control.Monad (mzero)
import GHC.Generics (Generic)
import Control.Lens ((^.))
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  parsedResponse <- WS.withSession getRedditList

  let body = parsedResponse ^. W.responseBody
      listing = children . datas $ body
      top10 = take 10 listing
      inner = map innerData top10

  let csvContents = Csv.encodeDefaultOrderedByName inner
  BSL.writeFile (outputFilename opts) csvContents


data RedditListing = RedditListing
  { kind :: Text
  , datas :: RedditListingData
  } deriving (Show)

instance FromJSON RedditListing where
  parseJSON (Aeson.Object v) =
    RedditListing <$> v .: "kind"
                  <*> v .: "data"
  parseJSON _ = mzero

data RedditListingData = RedditListingData
  { modhash :: Text
  , children :: [RedditListingItem]
  , after :: Text
  , before :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON RedditListingData

data RedditListingItem = RedditListingItem
  { innerKind :: Text
  , innerData :: RedditListingItemData
  } deriving (Show, Generic)

instance FromJSON RedditListingItem where
  parseJSON (Aeson.Object v) =
    RedditListingItem <$> v .: "kind"
                      <*> v .: "data"
  parseJSON _ = mzero

data RedditListingItemData = RedditListingItemData
  { title :: Text
  , subreddit :: Text
  , url :: Text
  , permalink :: Text
  } deriving (Show, Generic)

instance FromJSON RedditListingItemData

getRedditList :: WS.Session -> IO (W.Response RedditListing)
getRedditList session = do
  byteString <- WS.get session "https://reddit.com/hot.json"
  W.asJSON byteString

instance Csv.ToNamedRecord RedditListingItemData

instance Csv.DefaultOrdered RedditListingItemData

data Options = Options 
  { outputFilename :: String
  }

optsParser :: Parser Options
optsParser = Options
  <$> argument str
    ( metavar "FILENAME"
    <> help "File to output to"
    )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  ( fullDesc
  <> progDesc "The worst reddit client"
  <> header "redditcrawler - a bad reddit client"
  )

