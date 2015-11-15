module CPU.FunctionalUnits (select) where

import CLaSH.Prelude hiding (select)
import CPU.Defs (RobID)
import CPU.Op (Op(..))
import CPU.CDBMessage (CDBMessage(..), CDBData(..))

select :: KnownNat r => Op (RobID r) -> Index 3
select op = case op of
    Mov _ _   -> 0
    Add _ _ _ -> 0
    Ld  _ _   -> 1
    Jmp _     -> 2
    Jeq _ _ _ -> 2
    Halt      -> 2



--class FUState a where
--    ready :: a -> Bool

--data FPUState = FPUState

--instance FUState FPUState where
--    ready _ = True

--data LDUState = LDUState Read -- Pending read

--instance FUState LDUState where
--    ready (LDUState NoRead) = True
--    ready (LDUState Read)   = False

--data CUState = CUState

--instance FUState CUState where
--    ready = True

--fpu :: Maybe (RSEntry rob)
--    -> FPUState 
--    -> (FPUState, Maybe CDBMessage)
--fpu Nothing                 s = (,) s Nothing
--fpu (Just (RSEntry op rob)) s = (,) s $ Just $ CDBMessage rob $ case op of
--    Mov w rix -> RegWrite w rix
--    Add a b r -> case (a,b) of
--        (Literal aw, Literal bw) -> RegWrite (aw + bw) r
--        _                        -> error "Ungrounded add instruction"
--    _         -> error "Invalid instruction passed to FPU"

--ldu :: Maybe (RSEntry rob)
--    -> ¿MemRead?