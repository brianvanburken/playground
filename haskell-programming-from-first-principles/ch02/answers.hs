module Answers where

answer1 = x * 3 + y
  where x = 3
        y = 1000

answer2 = x * 5
  where y = 10
        x = 10 * 5 + y

answer3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

triple x = x * 3

waxOff x = triple x

waxOff2 x = c
  where a = triple x
        b = (^) a 2
        c = div b 10
