{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Config
  ( Config(..)
  ) where


import Data.Aeson
import GHC.Generics (Generic)

{-
 - Data type representing the configuration object required for interaction with twitter.
 -}
data Config = Config
  { apiKey :: String
  , apiSecret :: String
  , accessToken :: String
  , accessTokenSecret :: String
  } deriving (Show, Generic)


{-
 - We are expecting Configuraion to be a json file input so we must be able to convert that
 - json to a Config data type.
 - ToJSON is a type class. This line basically is all we need to set up for json
 - encoding an instance of our Config type
 -}
instance FromJSON Config

