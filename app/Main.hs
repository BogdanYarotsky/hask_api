{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Base64 as Base64
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

newtype ReqBody = ReqBody String
instance ToJSON ReqBody where
    toJSON (ReqBody query) = object
        [ 
            "query" .= query
        ]

reqBody :: ReqBody
reqBody = ReqBody "SELECT [System.Title], [System.State] FROM WorkItems"

-- GET request here to get all sprints
-- https://dev.azure.com/byarotsky/test/_apis/work/teamsettings/iterations?api-version=6.0
main :: IO ()
main = do
    let pat = ":pat_here"
    let request
         = setRequestHost "dev.azure.com"
         $ setRequestPath "/byarotsky/test/_apis/wit/wiql" 
         $ setRequestMethod "POST"
         $ setRequestHeader "Authorization" ["Basic " <> Base64.encode pat]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestQueryString [("api-version", Just "6.0")]
         $ setRequestBodyJSON reqBody
         $ setRequestSecure True
         $ setRequestPort 443
         $ defaultRequest

    print $ getRequestHeader "Authorization" request
    response <- httpJSON request
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)