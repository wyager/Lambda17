{-# LANGUAGE ScopedTypeVariables #-}

module CPU.RegisterFile (RegisterFile, renameReg, copyFrom) where

import CLaSH.Prelude
import CPU.Defs (RIx(..), RVal(Literal,Pending), StationID)
import CPU.Op (Op(..))

data RegisterFile fus stations = RegFile (Vec 16 (RVal (StationID fus stations)))

renameReg :: RIx -> StationID f s -> RegisterFile f s -> RegisterFile f s
renameReg (RIx rix) newName (RegFile vec) = RegFile $ replace rix (Pending newName) vec

copyFrom :: forall f s . RegisterFile f s -> Op RIx -> Op (StationID f s)
copyFrom (RegFile regs) op = case op of
        Halt      -> Halt
        Jmp pc    -> Jmp pc
        Mov w r   -> Mov w                     r
        Add a b r -> Add (update a) (update b) r
        Ld a r    -> Ld  (update a)            r
        Ldr a b r -> Ldr (update a) (update b) r
        Jeq a b p -> Jeq (update a) (update b) p 
    where
    update :: RVal RIx -> RVal (StationID f s)
    update (Literal w)   = Literal w
    update (Pending rix) = regs !! rix