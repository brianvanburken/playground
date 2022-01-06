module Main where

-- Exercises: Comprehend Thy Lists

-- It will output all even mySqr numbers:
-- Prelude> [x | x <- mySqr, rem x 2 == 0]
-- [4,16,36,64,100]

-- Prelude> :{
-- Prelude| [(x, y) | x <- mySqr,
-- Prelude|           y <- mySqr,
-- Prelude|           x < 50, y > 50]
-- Prelude| :}
-- [(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]

-- Prelude> :{
-- Prelude| take 5 [ (x, y) | x <- mySqr,
-- Prelude|                   y <- mySqr,
-- Prelude|                   x < 50, y > 50]
-- Prelude| :}
-- [(1,64),(1,81),(1,100),(4,64),(4,81)]

-- List comprehensions with Strings

-- Q: Given the above, what do you think this function would do:
--    Prelude> let myString xs = [x | x <- xs, elem x "aeiou"]
-- A: It will return only lowercase vowels defined in the string pased to `elem`

-- Exercise: Square Cube
let mySqr = [x^2 | x <- [1..5]]
let myCube = [y^3 | y <- [1..5]]

-- 1. First write an expression that will make tuples of the outputs of mySqr and myCube.
let myTuples = [(x, y) | x <- mySqr, y <- myCube]

-- 2. Now alter that expression so that it only uses the x and y values that are less than 50.
let myTuplesLessThanFifty = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3. Apply another function to that list comprehension to determine how many tuples inhabit your output list.
let myLength = length myTuplesLessThanFifty
