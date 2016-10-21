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
 - Decodes a given response object into a SearchResponse.
 -}
decodeTwRes :: Response BL.ByteString -> Either String SearchResponse
decodeTwRes =
  eitherDecode . responseBody


{-
 - Converts a given Tweet data type to a pretty string :D.
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
 - Preetfies a list of tweets :P. Atleast I find it pretty.
 -}
prettyTweets :: SearchResponse -> String
prettyTweets =
  concatMap prettyTweet . statuses


{-
 - Fetch tweets with the given query-string and with the given configuration object.
 -}
-- parse the url
-- create a request object
-- perform the request and get the response
tweetsContaining :: Manager -> Config -> String -> IO (Response BL.ByteString)
tweetsContaining manager config query = do
  url <- parseUrlThrow $ searchTweetsUrl ++ query
  req <- signWithConfig config url { method = "GET" }
  httpLbs req manager


{-
 - Get and display the tweets until the user presses the `j` key.
 -}
-- Continue to loop over and over again till users presses `j`.
-- <troll>Why `j` you ask? Bro do you even vim.</troll>
-- get response form twitter
-- if the response status is not 200 OK then error out.
-- else decode the response
-- Depending on the success/failure of the decoding of the response error out or
-- print the tweets.
-- then wait for user input again.
getTweets :: Manager -> Config -> String -> IO ()
getTweets manager config query = do
  response <- tweetsContaining manager config query

  if responseStatus response /= status200
    then
      BC.putStrLn . statusMessage $ responseStatus response
    else
      case decodeTwRes response of
        Left errMsg ->
          putStrLn errMsg

        Right twResponse -> do
          putStrLn $ prettyTweets twResponse

          case next $ meta twResponse of
            Nothing ->
              putStrLn "THATS ALL FOLKS"

            Just newUrl -> do
              putStrLn "j: More, <anything_else>: Exit"
              action <- getLine
              when (action == "j") $ getTweets manager config newUrl


{-
 - Starts the application.
 - * Takes in the file containing the configuration.
 - * The hashtag to be searched for
 - * The number of minimum retweets it should have
 -}
-- read the config file
-- if read fails the print the error.
-- otherwise create a new manager which keeps track of open-connections with default
-- Trasport Layer Security settings and continue to get tweets.
startApp :: String -> String -> Int -> IO ()
startApp fname hashtag minimumRetweets = do
  file <- configFromFile fname

  case file of
    Left errMsg -> do
      putStrLn errMsg
      putStr configErrMsg

    Right config -> do
      manager <- newManager tlsManagerSettings
      getTweets manager config (show $ Query hashtag minimumRetweets)

