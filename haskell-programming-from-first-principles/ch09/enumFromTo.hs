module EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool = generateList

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = generateList

eftInt :: Int -> Int -> [Int]
eftInt = generateList

eftChar :: Char -> Char -> [Char]
eftChar = generateList

generateList :: (Ord a, Enum a) => a -> a -> [a]
generateList current stop
    | current > stop = []
    | current == stop = [ current ]
    | otherwise = current : (generateList (succ current) stop)
