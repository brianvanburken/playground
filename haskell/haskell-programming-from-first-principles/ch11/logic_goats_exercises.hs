{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

-- 1. Reusing the TooMany typeclass, write an instance of the typeclass for the
-- type (Int, String). This will require adding a language pragma named
-- FlexibleInstances5 if you do not use a newtype — GHC will tell you what to do.

instance TooMany (Int, String) where
    tooMany (x, _) = tooMany x

instance TooMany (Int, Int) where
    tooMany (x, y) = tooMany (x + y)

instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany x && tooMany y
