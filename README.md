# tw-custserv

## Problem
Write a simple [Twitter API](https://dev.twitter.com/overview/documentation) client in Haskell. This client simply has to fetch and display
Tweets that:
- Have been re-Tweeted at least once and
- Contain the hashtag #custserv


## Using the application
- First, you must obtain consumer key and secret from [Twitter Application Management](https://apps.twitter.com/) page,
and you have to set those values to environment variables as shown below:
- Now you must prepare OAuth access token and secret
- Put all this information in a `config.json` file.

```json
{
  "apiKey": "<Consumer Key (API Key)>",
  "apiSecret": "<Consumer Secret (API Secret)>",
  "accessToken": "<Access Token>",
  "accessTokenSecret": "<Access Token Secret>"
}
```
