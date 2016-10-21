{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
  ( Status(..)
  , status200
  )

-- Qualified imports
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC



import Types.Config
import Types.Query
import TwitterAuth


{-
 - The base twitter url to search for tweets.
 -}
searchTweetsUrl :: String
searchTweetsUrl =
  "https://api.twitter.com/1.1/search/tweets.json"


{-
 - Fetch tweets with the given hash code and with the given configuration object.
 -}
tweetsContaining :: Config -> String -> IO (Response BL.ByteString)
tweetsContaining config query = do
  url <- parseUrlThrow $ searchTweetsUrl ++ query
  req <- signWithConfig config url { method = "GET" }
  manager <- newManager tlsManagerSettings
  httpLbs req manager


{-
 - Get and display the tweets until the user presses the `j` key.
 -}
getTweets :: Config -> String -> IO ()
getTweets config query = do
  response <- tweetsContaining config query
  let status = responseStatus response
  print status

  if status == status200
    then
      print $ responseBody response
    else
      BC.putStrLn $ statusMessage status



startApp :: IO ()
startApp = do
  file <- configFromFile "config.json"

  case file of
    Left errMsg -> do
      putStrLn errMsg
      putStr configErrMsg

    Right config ->
      getTweets config (show $ Query "custserv" 0)

