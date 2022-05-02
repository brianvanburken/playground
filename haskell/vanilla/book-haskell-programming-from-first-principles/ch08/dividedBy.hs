data DividedResult
    = Result Integer
    | DividedByZero

dividedBy :: (Integral a) => a -> a -> DividedResult
dividedBy num denom = go num denom 0
    where
        go n d count
            | d == 0 = DividedByZero
            | n < d = Result (count :: Integer)
            | otherwise =
                go (n - d) d (count + 1)
