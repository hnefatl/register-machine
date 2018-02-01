module RegisterMachine where

import Numeric.Natural

type Register = Natural -- Identify registers by their number
type Label = Natural    -- Identify instructions by their line

data Instruction = Halt | Incr Register Label | Decr Register Label Label

type Program = [Instruction] -- Just use lists, we're not all that fussed about efficiency