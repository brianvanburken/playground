module Exercises where

-- Exercises: Understanding Folds
-- 1. foldr (*) 1 [1..5]
-- b foldl (flip (*)) 1 [1..5]
-- c foldl (*) 1 [1..5]
question1 :: Int
question1 = foldr (*) 1 [1..5]

answer1B = optionB == question1
    where
        optionB :: Int
        optionB = foldl (flip (*)) 1 [1..5]

answer1C = optionC == question1
    where
        optionC :: Int
        optionC = foldl (*) 1 [1..5]


-- 2. Write out the evaluation steps for
-- foldl (flip (*)) 1 [1..3]
-- (((1 * 1) * 2) * 3)
--      ((1  * 2) * 3)
--            (2  * 3)
--                 (6)
--
-- foldl (flip (*)) ((flip (*)) 1 1) [2, 3]
-- foldl (flip (*)) ((flip (*)) 1 2) [3]
-- foldl (flip (*)) ((flip (*)) 2 3) []
-- foldl (flip (*)) 6 []
-- 6

-- 3. One difference between foldr and foldl is:
-- c) foldr, but not foldl, associates to the right

-- 4. Folds are catamorphisms, which means they are generally used to
-- a) reduce structure

-- 5. The following are simple folds very similar to what you’ve al- ready seen, but each has at least one error. Please fix them and test in your REPL:
-- a) foldr (++) ["woot", "WOOT", "woot"]
answer5A = foldr (++) "" ["woot", "WOOT", "woot"]

-- b) foldr max [] "fear is the little death"
answer5B = foldr max (minBound :: Char) "fear is the little death"

-- c) foldr and True [False, True]
answer5C = foldr (&&) True [False, True]

-- d) This one is more subtle than the previous. Can it ever return a different answer?  foldr (||) True [False, True]
answer5D = foldr (||) False [False, True]

-- e) foldl ((++) . show) "" [1..5]
answer5E = foldl (flip ((++) . show)) "" [1..5]

-- f) foldr const 'a' [1..5]
answer5F = foldr (flip const) 'a' [1..5]

-- g) foldr const 0 "tacos"
answer5G = foldr (flip const) 0 "tacos"

-- h) foldl (flip const) 0 "burritos"
answer5H = foldl const 0 "burritos"

-- i) foldl (flip const) 'z' [1..5]
answer5I = foldl const 'z' [1..5]
