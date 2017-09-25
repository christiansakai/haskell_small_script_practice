#!/usr/local/bin/stack
{- stack 
   exec runhaskell
   --resolver lts-9.3 
   --install-ghc 
   --package optparse-applicative
   --package aeson
   --package cassava
   --package postgresql-simple
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import System.Exit (exitFailure, exitSuccess)
import Data.Int (Int64)
import qualified Data.Text as T
import GHC.Word (Word16)
import qualified Database.PostgreSQL.Simple as Psql
import GHC.Generics (Generic)
import Data.Text (Text)
import           Data.Vector (Vector)
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as BSL
import System.IO
  ( hFlush
  , stdout
  )
import Options.Applicative
  ( Parser
  , ParserInfo
  , argument
  , auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , str
  , strOption
  , (<*>)
  )
import Data.Semigroup ((<>))


data Options = Options 
  { inputFilename :: String
  , dbHost :: String
  , dbPort :: Int
  , dbName :: String
  , dbUser :: String
  }

optsParser :: Parser Options
optsParser = Options
  <$> argument str
    ( metavar "FILE"
    <> help "input file"
    )
  <*> strOption
    ( long "dbhost"
    <> metavar "HOSTNAME"
    <> help "hostname of the database"
    )
  <*> option auto
    ( long "dbport"
    <> metavar "NUMBER"
    <> help "port of the database"
    )
  <*> strOption
    ( long "dbname"
    <> metavar "NAME"
    <> help "name of the database"
    )
  <*> strOption
    ( long "dbuser"
    <> metavar "USERNAME"
    <> help "user of the database"
    )

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser)
  ( fullDesc
  <> progDesc "A csv importer"
  <> header "dataImporter - a csv importer"
  )

main :: IO ()
main = do
  opts <- execParser optsParserInfo
  dbPass <- getPassword

  fileContents <- BSL.readFile $ inputFilename opts

  let csvContents = decodeCsv fileContents
  case csvContents of
    Left err -> do
      putStrLn err
      exitFailure

    Right (_, saleRecords) -> do
      conn <- makeConnection opts dbPass
      _ <- insertSaleRecords conn saleRecords
      exitSuccess

getPassword :: IO String
getPassword = do
  putStr "password: "
  dbPass <- getLine
  putStrLn ""
  return dbPass

data SaleRecord = SaleRecord
  { item :: Text
  , quantity :: Int
  , price :: Double
  } deriving (Generic)

instance Csv.FromNamedRecord SaleRecord

decodeCsv :: BSL.ByteString -> Either String (Csv.Header, Vector SaleRecord)
decodeCsv fileContents = Csv.decodeByName fileContents


makeConnection :: Options -> String -> IO Psql.Connection
makeConnection opts password = 
  Psql.connect (Psql.ConnectInfo 
                  (dbHost opts)
                  ((fromIntegral $ dbPort opts) :: Word16)
                  (dbUser opts)
                  password
                  (dbName opts)
               )

data Hole = Hole

insertSaleRecords :: Psql.Connection -> Vector SaleRecord -> IO (Vector Int64)
insertSaleRecords conn saleRecords =
  Psql.withTransaction conn (mapM (insertSaleRecord conn) saleRecords)

insertSaleRecord :: Psql.Connection -> SaleRecord -> IO Int64
insertSaleRecord conn saleRecord = 
  Psql.execute conn "insert into sales_record.sales_record (item, quantity, price) values (?, ?, ?)"
    [ item saleRecord
    , T.pack . show $ quantity saleRecord
    , T.pack . show $ price saleRecord
    ]

