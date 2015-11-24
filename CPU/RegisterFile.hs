{-# LANGUAGE ScopedTypeVariables #-}

module CPU.RegisterFile (RegisterFile(..), renameReg, copyFrom, empty) where

import CLaSH.Prelude hiding (empty)
import CPU.Defs (RIx(..), RVal(Literal,Pending), RobID)
import CPU.Op (Op(..))

data RegisterFile rob = RegFile (Vec 16 (RVal (RobID rob))) deriving (Show, Eq)

empty :: RegisterFile r
empty = RegFile $ repeat $ Literal 0x13371337

renameReg :: RIx -> RobID r -> RegisterFile r -> RegisterFile r
renameReg (RIx rix) newName (RegFile vec) = RegFile $ replace rix (Pending newName) vec

copyFrom :: forall r . RegisterFile r -> Op RIx -> Op (RobID r)
copyFrom (RegFile regs) op = case op of
        Halt      -> Halt
        Jmp pc    -> Jmp pc
        Mov w r   -> Mov w                     r
        Add a b r -> Add (update a) (update b) r
        Ld a r    -> Ld  (update a)            r
        Ldr a b r -> Ldr (update a) (update b) r
        Jeq a b p -> Jeq (update a) (update b) p 
    where
    update :: RVal RIx -> RVal (RobID r)
    update (Literal w)   = Literal w
    update (Pending rix) = regs !! rix