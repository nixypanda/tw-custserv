{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module Types.Tweets
  ( Tweet(..)
  , SearchResponse(..)
  , Meta(..)
  ) where


import Data.Aeson
import GHC.Generics (Generic)


-- The Single Tweet --

{-
 - Data type representing the configuration object required for interaction with twitter.
 -}
data Tweet = Tweet
  { _id :: Integer
  , text :: String
  , user :: String
  , retweetCount :: Integer
  } deriving (Show, Generic)


{-
 - We are expecting Tweet to be a json input so we must be able to convert that
 - json to a Tweet data type.
 - The actual respose is quite big but we only require the following information.
 - * The tweet itself.
 - * The retweet count (just to display it on the screen).
 - * The name of the user who tweeted (just for display purpose)
 -}
instance FromJSON Tweet where
  parseJSON (Object o) = Tweet
    <$> (o .: "id")
    <*> (o .: "text")
    <*> (o .: "user" >>= (.: "name"))
    <*> (o .: "retweet_count")

  parseJSON _ = mempty



-- Search api additional info --

{-
 - Contains the meta information about a search query.
 -
 - The complete respose contains the following (add more if necessary)
 - completed_in
 - max_id
 - max_id_str
 - query
 - refresh_url
 - next_results (the one currently required, its abasence means we are at the last page)
 - count
 - since_id
 - since_id_str
 -}
data Meta = Meta
  { next :: Maybe String
  } deriving (Show, Generic)


{-
 - Converting the (wild) JSON for meta info around search to a (tamed) Haskell data type.
 -}
instance FromJSON Meta where
  parseJSON (Object o) = Meta
    <$> (o .:? "next_results")

  parseJSON _ = mempty



-- The complete search response --

{-
 - The complete response form the twitter search api. Contains a list of tweets and some
 - meta information about the query that got performed.
 -}
data SearchResponse = SearchResponse
  { statuses :: [Tweet]
  , meta :: Meta
  } deriving (Show, Generic)


instance FromJSON SearchResponse where
  parseJSON (Object o) = SearchResponse
    <$> (o .: "statuses")
    <*> (o .: "search_metadata")

  parseJSON _ = mempty

