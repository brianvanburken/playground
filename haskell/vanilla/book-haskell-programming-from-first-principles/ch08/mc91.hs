mc91 :: (Ord a, Num a) => a -> a
mc91 n
    | n <= 100 = 91
    | otherwise = n - 10
