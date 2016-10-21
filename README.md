# tw-custserv

## Problem Statement
Write a simple [Twitter API](https://dev.twitter.com/overview/documentation) client in Haskell. This client simply has to fetch and display
Tweets that:
- Have been re-Tweeted at least once and
- Contain the hashtag #custserv


## Using the application
- First, you must obtain consumer key and secret from [Twitter Application Management](https://apps.twitter.com/) page,
and you have to set those values to the config json:
- create an application (fill in dummy data to the provided form)
- Now you must prepare OAuth access token and secret
- Under **Key and Access Tokens** tab. You will have *Application Setting* and *Your access token*
  generete them
- Put all this information in a `config.json` file.
  ```json
  {
    "apiKey": "<Consumer Key (API Key) in Application Setting>",
    "apiSecret": "<Consumer Secret (API Secret) in Application Setting>",
    "accessToken": "<Access Token in Your Access Token>",
    "accessTokenSecret": "<Access Token Secret in Your Access Token>"
  }
  ```
- run `stack bulid`
- run `stack exec tw-custserv-exe`


## About the code
- It is a stack project so all the things specific to stack apply here as well.
- `src/Types` contain all the custom types.
  - `Config.hs`: the configuration object used to authenticate.
  - `Query.hs`: the query object (extend it if you want more functionality from queries)
  - `Tweets.hs`: Representation of a *Tweet*, *Metadata accompnying a search query* and
    *The complete search response by twitter*
- `src/TwitterAuth.hs` contains all the Oauth related functions.
- `src/TwitterCustServ.hs` contains everythig else.


## Image
![App in action](https://raw.githubusercontent.com/jckdrpr/tw-custserv/master/resources/tw-custserv-exe.png)

## Issues
- User should be able to login with their credentials and not have to setup the config.json

