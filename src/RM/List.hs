module RM.List where

import Numeric.Natural
import Data.List (unfoldr)

import RM.Pure


encodeList :: [Natural] -> Natural
encodeList = foldr (curry encode1) 0

decodeList :: Natural -> [Natural]
decodeList = unfoldr (maybeDecode1)