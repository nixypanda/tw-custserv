module Main where

import TwitterCustServ

{-
 - All this does is call the start app function in Lib.
 -}
main :: IO ()
main =
  startApp "config.json" "custserv" 1

