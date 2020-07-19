f :: (Eq a, Num a) => a -> a
f n = go n 0
    where
        go :: (Eq a, Num a) => a -> a -> a
        go n sum
            | n == 0 = sum
            | otherwise =
                go (n - 1) (sum + n)
