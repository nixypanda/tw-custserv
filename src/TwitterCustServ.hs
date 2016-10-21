{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module TwitterCustServ
  ( startApp
  ) where


import Control.Monad (when)
import Data.Aeson

import Network.HTTP.Client
import Network.HTTP.Types
import Network.HTTP.Client.TLS

-- Qualified imports
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC


import Types.Config
import Types.Query
import Types.Tweets
import TwitterAuth


{-
 - The base twitter url to search for tweets.
 -}
searchTweetsUrl :: String
searchTweetsUrl =
  "https://api.twitter.com/1.1/search/tweets.json"

{-
 - Decodes a given response object.
 -}
decodeTwRes :: Response BL.ByteString -> Either String SearchResponse
decodeTwRes =
  eitherDecode . responseBody


{-
 - Converts a given Tweet data type to a pretty string.
 -}
prettyTweet :: Tweet -> String
prettyTweet Tweet{..} =
  let
    times = if retweetCount == 1 then " time" else " times"
  in
    unlines
      [ text
      , "-- " ++ user ++ "  (Retweeted " ++ show retweetCount ++ times ++ ")"
      , ""
      ]


{-
 - Preetfies a list of tweets.
 -}
prettyTweets :: SearchResponse -> String
prettyTweets =
  concatMap prettyTweet . statuses


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

  if responseStatus response == status200
    then
      case decodeTwRes response of
        Left errMsg ->
          putStrLn errMsg

        Right twResponse -> do
          putStrLn $ prettyTweets twResponse

          case next $ meta twResponse of
            Nothing -> do
              putStrLn "THATS ALL FOLKS"
              return ()

            Just newUrl -> do
              putStrLn "j: More, <anything_else>: Exit"
              action <- getLine
              when (action == "j") $ getTweets config newUrl

    else
      BC.putStrLn . statusMessage $ responseStatus response


{-
 - Starts the application.
 - * Takes in the file containing the configuration.
 - * The hashtag to be searched for
 - * The number of minimum retweets it should have
 -}
startApp :: String -> String -> Int -> IO ()
startApp fname hashtag minimumRetweets = do
  file <- configFromFile fname

  case file of
    Left errMsg -> do
      putStrLn errMsg
      putStr configErrMsg

    Right config ->
      getTweets config (show $ Query hashtag minimumRetweets)

