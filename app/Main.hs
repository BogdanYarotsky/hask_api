{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Base64 as Base64
import           Data.Aeson
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

newtype ReqBody = ReqBody String
instance ToJSON ReqBody where
    toJSON (ReqBody name) = object
        [ 
            "name" .= name
        ]

reqBody :: ReqBody
reqBody = ReqBody "SELECT [System.Title] FROM WorkItems"

main :: IO ()
main = do
    let pat = "pat"

    let request
         = setRequestHost "dev.azure.com"
         $ setRequestPath "/byarotsky/test/_apis/wit/wiql" 
         $ setRequestMethod "POST"
         $ setRequestHeader "Authorization" ["Basic " <> Base64.encode pat]
         $ setRequestQueryString [("api-version", Just "7.1")]
         $ setRequestBodyJSON reqBody
         $ setRequestSecure True
         $ setRequestPort 443
         $ defaultRequest

    print $ getRequestHeader "Authorization" request
    response <- httpJSON request
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)