module CPU.InstructionSet () where

import CPU.Defs(W(..))

data FetchedInstruction = Fetched Instruction PC

data Instruction = Mov W RIx
                 | Add RIx RIx RIx
                 | Jmp PC
                 | Halt
                 | Ld Addr RIx
                 | Ldr RIx RIx RIx
                 | Jeq RIx RIx PC

parse :: W -> Maybe Instruction
parse (W v) = case opcode of
    0 -> Just $ Mov (W $ zeroExtend $ slice d11 d4 v) t
    1 -> Just $ Add a b t
    2 -> Just $ Jmp (PC $ zeroExtend $ slice d11 d0 v)
    3 -> Just $ Halt
    4 -> Just $ Ld (Addr $ zeroExtend $ slice d11 d4 v) t
    5 -> Just $ Ldr a b t
    6 -> Just $ Jeq a b (PC $ zeroExtend $ slice d3 d0 v)
    _ -> Nothing
    where
    opcode = slice d15 d12 v
    a = RIx $ convert (slice d11 d8 v)
    b = RIx $ convert (slice d7  d4 v)
    t = RIx $ convert (slice d3  d0 v)
    convert = toEnum . fromEnum