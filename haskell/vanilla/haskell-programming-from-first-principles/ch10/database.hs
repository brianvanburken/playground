module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
        deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

-- 1. Write a function that filters for DbDate values and returns a list
-- of the UTCTime values inside them.
-- Alternative solution:
-- filterDbDate db = [time | (DbDate time) <- db]
filterDbDate :: [ DatabaseItem ] -> [ UTCTime ]
filterDbDate =
    foldr filter []
        where
            filter :: DatabaseItem -> [ UTCTime ] -> [ UTCTime ]
            filter (DbDate value) acc = acc ++ [value]
            filter _ acc = acc

-- 2. Write a function that filters for DbNumber values and returns a list
-- of the Integer values inside them.
-- Alternative solution:
-- filterDbDate db = [number | (DbNumber number) <- db]
filterDbNumber :: [ DatabaseItem ] -> [ Integer ]
filterDbNumber =
    foldr filter []
        where
            filter :: DatabaseItem -> [ Integer ] -> [ Integer ]
            filter (DbNumber value) acc = acc ++ [value]
            filter _ acc = acc

-- 3. Write a function that gets the most recent date.
-- We could cheat using max: mostRecent = filterDbDate . max
mostRecent :: [ DatabaseItem ] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the DbNumber values.
avgDb :: [DatabaseItem] -> Double
avgDb db = if count == 0 then 0 else (fromIntegral total) / (fromIntegral count)
    where
        count = (length . filterDbNumber) db
        total = sumDb db
