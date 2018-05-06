module Answers where

appendExclamationmark :: String -> String
appendExclamationmark str =
    str ++ "!"

takeFourthCharacter :: String -> String
takeFourthCharacter str =
    take 1 (drop 4 str)

getFromNinthCharacter :: String -> String
getFromNinthCharacter =
    (drop 9)

rvrs :: String
rvrs =
    concat [awesome, " ", is, " ", curry, "."]
        where sentence = "Curry is awesome"
              curry = take 5 sentence
              is = take 2 (drop 6 sentence)
              awesome = take 7 (drop 9 sentence)
