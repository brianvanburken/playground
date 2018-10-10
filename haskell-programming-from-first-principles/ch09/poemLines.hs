module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
          \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines txt
    | txt == [] = []
    | otherwise = (takeWhile notNewLine txt) : myLines ((dropWhile isNewLine . dropWhile notNewLine) txt)

notNewLine :: Char -> Bool
notNewLine = not . isNewLine

isNewLine :: Char -> Bool
isNewLine c
    | c == '\n' = True
    | otherwise = False

shouldEqual =
       [ "Tyger Tyger, burning bright"
       , "In the forests of the night"
       , "What immortal hand or eye"
       , "Could frame thy fearful symmetry?"
       ]

main :: IO ()
main = print $
       "Are they equal? " ++ show (myLines sentences == shouldEqual)
