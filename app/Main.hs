{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import Data.Aeson
import           GHC.Generics

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8

newtype WorkItemsRequestBody = WorkItemsRequestBody {
    query :: String
} deriving (Generic, Show)
instance ToJSON WorkItemsRequestBody

workItemsReqBody :: WorkItemsRequestBody
workItemsReqBody = WorkItemsRequestBody "SELECT [System.Title], [System.State] FROM WorkItems"


-- GET request here to get all sprints
-- https://dev.azure.com/byarotsky/test/_apis/work/teamsettings/iterations?api-version=6.0


data Config = Config {
  organization :: String,
  project :: String,
  pat :: String
} deriving (Generic, Show)
instance FromJSON Config

main :: IO ()
main = do
    config <- BL.readFile "config.json"
    case (decode config :: Maybe Config) of
        Just cfg -> main' cfg
        Nothing -> putStrLn "could not read config file!"

main' :: Config -> IO ()
main' config = do
    let encodedPat 
         = Base64.encode 
         $ C8.pack 
         $ ":" ++ pat config

    let authHeader = "Basic " <> encodedPat
    C8.putStrLn authHeader


    -- let request
    --      = setRequestHost "dev.azure.com"
    --      $ setRequestPath "/byarotsky/test/_apis/wit/wiql"
    --      $ setRequestMethod "POST"
    --      $ setRequestHeader "Authorization" [authHeader]
    --      $ setRequestHeader "Content-Type" ["application/json"]
    --      $ setRequestQueryString [("api-version", Just "6.0")]
    --      $ setRequestBodyJSON workItemsReqBody
    --      $ setRequestSecure True
    --      $ setRequestPort 443
    --      defaultRequest

    -- print $ getRequestHeader "Authorization" request
    -- response <- httpJSON request
    -- C8.putStrLn $ Yaml.encode (getResponseBody response :: Value)