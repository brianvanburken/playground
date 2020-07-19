module Chiper where

import Data.Char

characterZ :: Int
characterZ = ord 'z'

characterA :: Int
characterA = ord 'a'

ceasar :: Int -> String -> String
ceasar _ "" = ""
ceasar shiftSize (x:xs) = shiftRight shiftSize x : ceasar shiftSize xs

unCeasar :: Int -> String -> String
unCeasar _ "" = ""
unCeasar shiftSize (x:xs) = shiftLeft shiftSize x : unCeasar shiftSize xs

shiftRight :: Int -> Char -> Char
shiftRight shiftSize char =
    if isPastCharacterZ then
        chr (characterA + (nextCharPosition - characterZ - 1))
    else
        chr nextCharPosition
    where
        lowercaseChar :: Char
        lowercaseChar = toLower char

        nextCharPosition :: Int
        nextCharPosition = ord lowercaseChar + shiftSize

        isPastCharacterZ :: Bool
        isPastCharacterZ = nextCharPosition > characterZ

shiftLeft :: Int -> Char -> Char
shiftLeft shiftSize char =
    if isPastCharacterA then
        chr (characterZ - (characterA - nextCharPosition - 1))
    else
        chr nextCharPosition
    where
        lowercaseChar :: Char
        lowercaseChar = toLower char

        nextCharPosition :: Int
        nextCharPosition = ord lowercaseChar - shiftSize

        isPastCharacterA :: Bool
        isPastCharacterA = nextCharPosition < characterA
