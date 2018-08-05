module WordNumber where

import Data.List (intersperse, concat)
import Data.Char (digitToInt)

digitToWord :: Int -> String
digitToWord =
    concat . (intersperse "-") . (map wordNumber) . digits

digits :: Int -> [ Int ]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

wordNumber :: Int -> String
wordNumber 0 = "zero"
wordNumber 1 = "one"
wordNumber 2 = "two"
wordNumber 3 = "three"
wordNumber 4 = "four"
wordNumber 5 = "five"
wordNumber 6 = "six"
wordNumber 7 = "seven"
wordNumber 8 = "eight"
wordNumber 9 = "nine"
wordNumber n = ""
