module CPU.OpBuffer (OpBuffer, insert, take, empty) where

import CLaSH.Prelude hiding (take, empty)
import qualified CPU.Buffer as Buf
import CPU.Op (Op, Fetched(..))
import CPU.Defs (PC, Predicted(..), RIx)

data OpBuffer n = IB {fetch_pc :: PC, buf :: Buf.Buffer n (Fetched (Op RIx))} deriving (Show, Eq)

empty :: KnownNat n => OpBuffer n
empty = IB 0 Buf.empty

insert :: KnownNat n => OpBuffer n -> Fetched (Op RIx) -> OpBuffer n
insert ib@(IB _ buf)        _ | Buf.full buf   = ib -- Can't insert anything!
insert ib@(IB fetch_pc buf) fetched@(Fetched pc (Predicted pc') _)
                              | pc == fetch_pc = IB pc' (Buf.insert' buf fetched)
                              | otherwise      = ib -- Incorrect fetch!

-- Don't touch fetch_pc. Only an insert should do that.
take :: KnownNat (n+1) => OpBuffer (n+1) -> (OpBuffer (n+1), Maybe (Fetched (Op RIx)))
take (IB fetch_pc buf) = let (buf', result) = Buf.take buf 
                         in (IB fetch_pc buf', result)


