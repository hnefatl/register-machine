module RM.Pure
(
    encode1,
    decode1,
    maybeDecode1,

    encode0,
    decode0,
) where

import Numeric.Natural


-- <<x,y>> -> 2^x(2y+1)
encode1 :: (Natural, Natural) -> Natural
encode1 (x, y) = 2^x * (2*y + 1)

-- <x,y> -> <<x,y>> - 1
encode0 :: (Natural, Natural) -> Natural
encode0 p = encode1 p - 1

-- 
decode1 :: Natural -> (Natural, Natural)
decode1 x = case maybeDecode1 x of
        Nothing -> error "decode1 not defined for input 0"
        Just p  -> p

maybeDecode1 :: Natural -> Maybe (Natural, Natural)
maybeDecode1 0 = Nothing
maybeDecode1 z = Just (x, y)
    where (x, p) = greatestDiv z
          y = (z `div` p) `div` 2


decode0 :: Natural -> (Natural, Natural)
decode0 x = decode1 (x + 1)

-- Return the greatest power of 2 which divides the number
greatestDiv :: Natural -> (Natural, Natural)
greatestDiv x = last $ zip [0..] $ takeWhile (\d -> x `mod` d == 0) $ iterate (*2) 1