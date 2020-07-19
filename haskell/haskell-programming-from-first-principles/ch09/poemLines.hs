module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
          \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines = (splitOn '\n')

splitOn :: Char -> String -> [ String ]
splitOn char str
    | str == [] = []
    | otherwise = (takeWhile (not . isChar char) str) : splitOn char ((dropWhile (isChar char) . dropWhile (not . isChar char)) str)

isChar :: Char -> Char -> Bool
isChar check = (== check)

shouldEqual =
       [ "Tyger Tyger, burning bright"
       , "In the forests of the night"
       , "What immortal hand or eye"
       , "Could frame thy fearful symmetry?"
       ]

main :: IO ()
main = print $
       "Are they equal? " ++ show (myLines sentences == shouldEqual)
