f :: (Integral a) => a -> a -> a
f n1 n2 = go n1 n2 0
    where
        go :: (Integral a) => a -> a -> a -> a
        go n1 n2 sum
            | n2 == 0 = sum
            | otherwise =
                go n1 (n2 - 1) (sum + n1)
