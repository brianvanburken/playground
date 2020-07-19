module MyWords where

myWord :: String -> [ String ]
myWord = (splitOn ' ')

splitOn :: Char -> String -> [ String ]
splitOn char str
    | str == [] = []
    | otherwise = (takeWhile (not . isChar char) str) : splitOn char ((dropWhile (isChar char) . dropWhile (not . isChar char)) str)

isChar :: Char -> Char -> Bool
isChar check = (== check)
