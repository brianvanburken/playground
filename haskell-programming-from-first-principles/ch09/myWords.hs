module MyWords where

myWord :: String -> [ String ]
myWord str
    | str == [] = []
    | otherwise =  (takeWhile notSpace str) : myWord ((dropWhile isSpace . dropWhile notSpace) str)

notSpace :: Char -> Bool
notSpace = not . isSpace

isSpace :: Char -> Bool
isSpace c
    | c == ' ' = True
    | otherwise = False
