{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import Data.Aeson
import           GHC.Generics

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Data.List

-- Define the data structure for iteration attributes
data IterationAttributes = IterationAttributes {
    startDate  :: Maybe String,
    finishDate :: Maybe String,
    timeFrame  :: String
} deriving (Show, Generic)
instance FromJSON IterationAttributes

-- Define the data structure for each iteration
data Iteration = Iteration {
    iterationId :: String,
    name       :: String,
    path       :: String,
    attributes :: IterationAttributes,
    url        :: String
} deriving (Show, Generic)
instance FromJSON Iteration

-- Define the data structure for the JSON response
data IterationsResponse = IterationsResponse {
    count :: Int,
    value :: [Iteration]
} deriving (Show, Generic)
instance FromJSON IterationsResponse


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
        Nothing -> putStrLn "could not read config file"

main' :: Config -> IO ()
main' config = do
    let devOpsRequest = defaultDevOpsRequest $ pat config
    let baseReqestPath = C8.pack $ concat [organization config, "/", project config, "/_apis"]

    let iterationsRequest
         = setRequestPath (baseReqestPath <> "/work/teamsettings/iterations")
         $ setRequestMethod "GET"
         devOpsRequest

    iterationsResponse <- httpJSON iterationsRequest
    let body = getResponseBody iterationsResponse :: IterationsResponse
    C8.putStrLn $ C8.pack $ show $ count body

findCurrentIterationId :: IterationsResponse -> Maybe String
findCurrentIterationId (IterationsResponse _ iterations)
    =  iterationId <$> findCurrentIteration iterations
    where findCurrentIteration iters = Just $ head iters


defaultDevOpsRequest :: String -> Request
defaultDevOpsRequest password
         = setRequestHost "dev.azure.com"
         $ setRequestBasicAuth "" (C8.pack password)
         $ setRequestQueryString [("api-version", Just "6.0")]
         $ setRequestSecure True
         $ setRequestPort 443
         defaultRequest

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