module Reverse where

rvrs :: String -> String
rvrs x =
    concat [awesome, " ", is, " ", curry, "."]
        where curry = take 5 x
              is = take 2 (drop 6 x)
              awesome = take 7 (drop 9 x)


main :: IO ()
main = print $ rvrs "Curry is awesome"
