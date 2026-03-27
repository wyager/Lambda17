{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module CPU.Op (Fetched(..), Op(..), grounded, convert) where

import Clash.Prelude 
import CPU.Defs (PC(..), Predicted(..), RIx(..), RVal(..), W(..), Addr(..), StationID(..))
import qualified CPU.InstructionSet as I

data Fetched op = Fetched PC (Predicted PC) op deriving (Show, Eq, Generic, NFDataX)

-- rix is either RIx (for instrs still in the buffer)
-- or RobID (for instrs that have been dispatched)
--
-- Nop is a pseudo-op used only in the ROB for the virtual Add half of
-- an Ldr decomposition. The Add stays in the reservation station and
-- broadcasts on the CDB so the Ld can pick up the computed address, but
-- the ROB slot holds a Nop so commit does not write to a real register.
-- Without this, the virtual Add's result would clobber backup register 0.
data Op rix = Mov W RIx
            | Add (RVal rix) (RVal rix) RIx
            | Jmp PC
            | Halt
            | Ld  (RVal rix) RIx
            | Ldr (RVal rix) (RVal rix) RIx
            | Jeq (RVal rix) (RVal rix) PC
            | Nop
            deriving (Eq, Show, Generic, NFDataX)

grounded :: Op a -> Bool
grounded op = case op of
    Mov _ _                       -> True
    Add (Literal _) (Literal _) _ -> True
    Jmp _                         -> True
    Halt                          -> True
    Ld  (Literal _) _             -> True
    Jeq (Literal _) (Literal _) _ -> True
    Ldr (Literal _) (Literal _) _ -> True
    Nop                           -> True
    _                             -> False

convert :: I.FetchedInstruction -> Fetched (Op RIx)
convert (I.Fetched instr from) = case instr of
    I.Halt      -> simple $ Halt
    I.Jmp pc    -> jmp pc $ Jmp pc
    I.Mov w r   -> simple $ Mov w                r
    I.Ld a r    -> simple $ Ld  (addr a)         r -- Src is non-grounded so we can decompose Ldr into Add + Ld
    I.Add a b c -> simple $ Add (src a)  (src b) c
    I.Ldr a b c -> simple $ Ldr (src a)  (src b) c
    I.Jeq a b p -> simple $ Jeq (src a)  (src b) (p + from)
    where
    src  rix   = Pending rix
    addr (Addr a)  = Literal $ W a
    simple = Fetched from (Predicted (from + 1))
    jmp pc = Fetched from (Predicted pc)