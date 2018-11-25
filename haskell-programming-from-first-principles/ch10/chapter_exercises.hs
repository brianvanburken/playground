module ChapterExercises where

stops = "pbtdkg"
vowels = "aeiou"

-- 1a: Write a function that takes inputs from stops and vowels and makes
-- 3-tuples of all possible stop-vowel-stop com- binations. These will not all
-- correspond to real words in English, although the stop-vowel-stop pattern is
-- common enough that many of them will.
stopVowelStop :: [ (Char, Char, Char) ]
stopVowelStop =
    [ (stop1, vowel, stop2) | stop1 <- stops, vowel <- vowels, stop2 <- stops ]

-- 1b: Modify that function so that it only returns the combinations that begin
-- with a p.
startWithP :: [ (Char, Char, Char) ]
startWithP =
    [ ('p', vowel, stop2) | vowel <- vowels, stop2 <- stops ]

-- 1c: Now set up lists of nouns and verbs (instead of stops and vowels) and
-- modify the function to make tuples represent- ing possible noun-verb-noun
-- sentences.
nouns = ["cat", "dog", "human"]
verbs = ["eats", "loves", "pets"]
nounVerbNoun :: [(String, String, String)]
nounVerbNoun = [ (x, y, z) | x <- nouns, y <- verbs, z <- nouns ]

nvnSentence :: [String]
nvnSentence = [ x ++ " " ++ y ++ " " ++ z | x <- nouns, y <- verbs, z <- nouns ]

-- 2 What does the following mystery function do? What is its type? Try to get
-- a good sense of what it does before you test it in the REPL to verify it.
-- A: It get all the words and calculates the average length of the letters used
-- per word.
seekritFunc :: String -> Int
seekritFunc x =
    div (sum (map length (words x)))
               (length (words x))

-- 3 We’d really like the answer to be more precise. Can you rewrite that using
-- fractional division?-- a good sense of what it does before you test it in the REPL to verify it.
seekritFunc' :: String -> Double
seekritFunc' x =
    fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds
-- 1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny returns True if a -> Bool applied to any of the values in the
-- list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- Example for validating myAny:
--      Prelude> myAny even [1, 3, 5]
--      False
--      Prelude> myAny odd [1, 3, 5]
--      True

-- 3. Write two versions of myElem. One version should use folding and the
-- other should use any.
myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny ((==) x)

     -- Prelude> myElem 1 [1..10]
     -- True
     -- Prelude> myElem 1 [2..10]
     -- False

-- 4. Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

     -- Prelude> myReverse "blah"
     -- "halb"
     -- Prelude> myReverse [1..5]
     -- [5,4,3,2,1]

-- 5. Write myMap in terms of foldr. It should have the same behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6. Write myFilter in terms of foldr. It should have the same behav-
-- ior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []

-- 7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the re- sults.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

     -- Prelude> squishMap (\x -> [1, x, 3]) [2]
     -- [1,2,3]
     -- Prelude> let f x = "WO " ++ [x] ++ " OT "
     -- Prelude> squishMap f "blah"
     -- "WO b OT WO l OT WO a OT WO h OT "


-- 9. squishAgain flattens a list of lists into a list. This time re-use the
-- squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldr (\a b ->
    case f a b of
      GT -> a
      _  -> b) x xs

     -- Prelude> myMaximumBy (\_ _ -> GT) [1..10]
     -- 1
     -- Prelude> myMaximumBy (\_ _ -> LT) [1..10]
     -- 10
     -- Prelude> myMaximumBy compare [1..10]
     -- 10

-- 11. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldr (\a b ->
    case f a b of
      LT -> a
      _  -> b) x xs

     -- Prelude> myMinimumBy (\_ _ -> GT) [1..10]
     -- 10
     -- Prelude> myMinimumBy (\_ _ -> LT) [1..10]
     -- 1
     -- Prelude> myMinimumBy compare [1..10]
     -- 1
