module OpBuffer () where

import CLaSH.Prelude
import qualified CPU.Buffer as Buf
import CPU.InstructionSet (Instruction)
import CPU.Op (Op)

data InstBuffer n = IB {fetch_pc :: PC, buf :: Buffer n (Fetched Op)}

empty :: InstBuffer n
empty = IB 0 Buf.empty

insert :: InstBuffer n -> Fetched Op -> InstBuffer n
insert ib@(IB _ buf)        _ | full buf       = ib -- Can't insert anything!
insert ib@(IB fetch_pc buf) (Fetched pc (Predicted pc') op)
                              | pc == fetch_pc = IB pc' (Buf.insert' buf op)
                              | otherwise      = ib -- Incorrect fetch!

-- Don't touch fetch_pc. Only an insert should do that.
take :: InstBuffer (n+1) -> (InstBuffer (n+1), Maybe (Fetched Op))
take (IB fetch_pc buf) = let (buf', result) = Buf.take buf 
                         in (IB fetch_pc buf', result)


