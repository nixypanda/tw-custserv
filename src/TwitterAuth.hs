{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module TwitterAuth
  ( configErrMsg
  , configFromFile
  , signWithConfig
  ) where


import Data.Aeson (eitherDecode)
import Data.ByteString (ByteString)
import Network.HTTP.Client
import Web.Authenticate.OAuth
  ( OAuth(..)
  , OAuthVersion(..)
  , SignMethod(..)
  , newCredential
  , newOAuth
  , signOAuth
  )

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Types.Config


-- Ad-hoc type declarations

{-
 - The consumer key.
 - (NOTE: Just to make oauthTwitter type signature more readable)
 -}
type ConsumerKey =
  ByteString


{-
 - The secret key.
 - (NOTE: Just to make oauthTwitter type signature more readable)
 -}
type SecretKey =
  ByteString


{-
 - Here we pass in our OAuth consumer key and secret to build an OAuth; these correspond to the
 - Twitter API key/secret, and is one half of what we need to fully authenticate Twitter requests.
 -}
oauthTwitter :: ConsumerKey -> SecretKey -> OAuth
oauthTwitter key secret =
  newOAuth
    { oauthServerName = "twitter"
    , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
    , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
    , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
    , oauthSignatureMethod = HMACSHA1
    , oauthConsumerKey = key
    , oauthConsumerSecret = secret
    , oauthVersion = OAuth10a
    , oauthCallback = Just (B.pack "http://jckdrpr.github.io/tw-cust-serv/")
    }


{-
 - Given a filepath read in Config data type.
 -}
configFromFile :: FilePath -> IO (Either String Config)
configFromFile path = do
  contents <- BL.readFile path
  return $ eitherDecode contents


{- 
 - We pass in our OAuth consumer key and secret to build an OAuth; these correspond to the
 - Twitter API key/secret, and is one half of what we need to fully authenticate Twitter requests.
 -
 - The other half is a Credential, which we can build with newCredential using our user key and secret.
 - We can fully sign an arbitrary request using a Config.
 -
 - All this with the Request object will create an IO Request.
 -}
signWithConfig :: Config -> Request -> IO Request
signWithConfig Config{..} =
  signOAuth
    (oauthTwitter (B.pack apiKey) (B.pack apiSecret))
    (newCredential (B.pack accessToken) (B.pack accessTokenSecret))


{-
 - Helpful error message in case the provided config.json is invalid.
 -}
configErrMsg :: String
configErrMsg =
  unlines
    [ "Please make sure the config looks as follows:"
    , "{"
    , "\t\"apiKey\": \"<api key>\","
    , "\t\"apiSecret\": \"<api secret>\","
    , "\t\"accessToken\": \"<access token>\","
    , "\t\"accessTokenSecret\": \"<access token secret>\""
    , "}"
    ]

