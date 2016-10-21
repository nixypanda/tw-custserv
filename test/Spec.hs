import Types.Query
import Test.HUnit


verifyQueryShow :: Test
verifyQueryShow =
  let
    expected = "?q=custserv%20min_retweets%3A1"
    actual = show $ Query "custserv" 1
    label = "properly converts Query object to string"
  in 
    TestCase (assertEqual label expected actual)


tests :: Test
tests =
  TestList
    [ TestLabel "properly converts Query object to string" verifyQueryShow
    ]


main :: IO ()
main = do
  cs <- runTestTT tests
  print cs

