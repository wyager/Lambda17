import CPU.Dispatch
import CPU.OpBuffer
import CPU.Op (Op(..), Fetched(..),)
import CPU.Defs (Predicted(..), RVal(..))
import CPU.FunctionalUnits (select)
import CLaSH.Prelude hiding (select)

s0 = CPU.Dispatch.empty :: DispatchState 5 3 2 10

insertOp s o = s {opBuffer = CPU.OpBuffer.insert (opBuffer s) o}

s1 = insertOp s0 (Fetched 0 (Predicted 1) (Mov 5 5))

s2 = insertOp s1 (Fetched 1 (Predicted 2) (Ldr (Pending 5) (Pending 5) 7))

dispatch2 select s = dispatch select s >>= dispatch select