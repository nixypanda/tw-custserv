{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import qualified Data.ByteString.Lazy as BL

import Types.Config
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


startApp :: IO ()
startApp = do
  file <- configFromFile "config.json"

  case file of
    Left errMsg -> do
      putStrLn errMsg
      putStr configErrMsg

    Right config -> do
      res <- tweetsContaining config ""
      print res

