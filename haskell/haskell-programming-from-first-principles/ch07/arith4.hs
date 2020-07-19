-- arith4.hs
module Arith4 where

-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print (roundTrip 4 :: Int)
  print (id 4)
