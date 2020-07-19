module ChapterExercises where

import Data.Char

-- 1. Query the types of isUpper and toUpper.
-- :t isUpper
-- :t toUpper

-- 2. Given the following behaviors, which would we use to write a function that filters all the uppercase letters out of a String? Write that function such that, given the input “HbEfLrLxO,” your function will return “HELLO.”
--   Prelude Data.Char> isUpper 'J'
--   True
--   Prelude Data.Char> toUpper 'j'
--   'J'
filterUpper :: String -> String
filterUpper = filter isUpper
-- filterUpper "HbEfLrLxO" -- "HELLO"


-- 3. Write a function that will capitalize the first letter of a string and
-- return the entire string. For example, if given the argument
-- “julie,” it will return “Julie.”
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs
-- capitalize "jullie" -- "Jullie"

-- 4. Now make a new version of that function that is recursive such that if you give it the input “woot” it will holler back at you “WOOT.” The type signature won’t change, but you will want to
-- add a base case.
allUppercase :: String -> String
allUppercase "" = ""
allUppercase (x:xs) = toUpper x : allUppercase xs
-- allCaps "woot" -- "WOOT"

-- 5. To do the final exercise in this section, we’ll need another stan- dard function for lists called head. Query the type of head and experiment with it to see what it does. Now write a function that will capitalize the first letter of a String and return only that letter as the result.
-- 6. Cool. Good work. Now rewrite it as a composed function. Then, for fun, rewrite it pointfree.
capital :: String -> Char
capital = toUpper . head
-- capital "jullie" -- 'J'
