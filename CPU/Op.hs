module CPU.Op (Fetched(..), Op(..), grounded, convert) where

import CLaSH.Prelude 
import CPU.Defs (PC(..), Predicted(..), RIx(..), RVal(..), W(..), Addr(..), StationID(..))
import qualified CPU.InstructionSet as I

data Fetched op = Fetched PC (Predicted PC) op deriving (Show, Eq)

-- rix is either RIx (for instrs still in the buffer)
-- or RobID (for instrs that have been dispatched)
data Op rix = Mov W RIx
            | Add (RVal rix) (RVal rix) RIx
            | Jmp PC
            | Halt
            | Ld  (RVal rix) RIx
            | Ldr (RVal rix) (RVal rix) RIx
            | Jeq (RVal rix) (RVal rix) PC
            deriving (Eq, Show)

grounded :: Op a -> Bool
grounded op = case op of
    Mov _ _                       -> True
    Add (Literal _) (Literal _) _ -> True
    Jmp _                         -> True
    Halt                          -> True
    Ld  (Literal _) _             -> True
    Jeq (Literal _) (Literal _) _ -> True
    Ldr (Literal _) (Literal _) _ -> True
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