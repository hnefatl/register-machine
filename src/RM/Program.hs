module RM.Program where

import Numeric.Natural

import RM.Pure
import RM.List


type Register = Natural -- Identify registers by their number
type Label = Natural    -- Identify instructions by their line

data Instruction = Halt | Incr Register Label | Decr Register Label Label

type Program = [Instruction] -- Just use lists, we're not all that fussed about efficiency


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