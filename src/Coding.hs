module Coding
(
    encode1,
    decode1,
    maybeDecode1,

    encode0,
    decode0,

    encodeList,
    decodeList
) where

import Numeric.Natural
import Data.List (unfoldr)

import RegisterMachine

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


encodeList :: [Natural] -> Natural
encodeList = foldr (curry encode1) 0

decodeList :: Natural -> [Natural]
decodeList = unfoldr (maybeDecode1)


encodeInstruction :: Instruction -> Natural
encodeInstruction Halt = 0
encodeInstruction (Incr r l) = encode1 (2*r, l)
encodeInstruction (Decr r l1 l2) = encode1 (2*r+1, encode0 (l1, l2))

decodeInstruction :: Natural -> Instruction
decodeInstruction 0 = Halt
decodeInstruction i =
            let
                (rc, l) = decode1 i  -- Get register code and label code (maybe actual label, maybe another pair)
                r = r `div` 2        -- Get actual register
                (l1, l2) = decode0 l -- If it's a Decr instruction, there's two encoded labels
            in
                if even rc then Incr r l
                else Decr r l1 l2

encodeProgram :: Program -> Natural
encodeProgram = encodeList . map encodeInstruction

decodeProgram :: Natural -> Program
decodeProgram = map decodeInstruction . decodeList