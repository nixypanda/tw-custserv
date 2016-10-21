{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Types.Query
  ( Query(..)
  , humanRedable
  ) where


{-
 - Data type representing the search object required for interaction with twitter.
 - The twitter search API is super undocumented.
 - Anyway https://dev.twitter.com/rest/public/search will help in how to form a
 - query.
 -}
data Query = Query
  { hash :: String
  , minRetweets :: Int
  } deriving (Eq)


{-
 - Creating a url extension form the given values.
 -}
instance Show Query where
  show Query{..} =
    "?q=" ++ hash ++ "%20min_retweets%3A" ++ show minRetweets


{-
 - A human Readable version of what the query will look like.
 -}
humanRedable :: Query -> String
humanRedable Query{..} =
  "#" ++ hash ++ " min_retweets:" ++ show minRetweets

