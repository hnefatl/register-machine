module Coding where

import Numeric.Natural

-- <<x,y>> -> 2^x(2y+1)
encode1 :: (Natural, Natural) -> Natural
encode1 (x, y) = 2^x * (2*y + 1)

-- <x,y> -> <<x,y>> - 1
encode0 :: (Natural, Natural) -> Natural
encode0 p = encode1 p - 1

-- 
decode1 :: Natural -> (Natural, Natural)
decode1 0 = error "decode1 not defined for input 0"
decode1 z = (x, y)
    where (x, p) = greatestDiv z
          y = (z `div` p) `div` 2

decode0 :: Natural -> (Natural, Natural)
decode0 x = decode1 (x + 1)

-- Return the greatest power of 2 which divides the number
greatestDiv :: Natural -> (Natural, Natural)
greatestDiv x = last $ zip [0..] $ takeWhile (\d -> x `mod` d == 0) $ iterate (*2) 1