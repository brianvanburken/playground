module MyWords where

myWord :: String -> [ String ]
myWord str
    | str == [] = []
    | otherwise =  takeWhile (/= ' ') str : myWord (dropWhile (== ' ') (dropWhile (/= ' ') str))
