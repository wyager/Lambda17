{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Dispatch () where

import CLaSH.Prelude hiding (take)
import Data.Maybe (fromJust)
import CPU.Defs (RVal(Pending, Literal), RIx, Predicted(..), StationID(..))
import CPU.Op (Fetched(..), Op(..))
import CPU.RegisterFile (RegisterFile, renameReg, copyFrom)
import CPU.OpBuffer (OpBuffer, take)
import CPU.RStations (RStations, freeSlot, insert)
import CPU.ReorderBuffer (ROB, oneFree, twoFree, waitFor)

data DistpatchState n f s d = DS (OpBuffer (n+1)) (RegisterFile f s) (RStations f s) (ROB d f s)

-- We return left if it failed, right if it succeeded. This way,
-- we can write a do-expression to update dispatch with multiple instructions
-- and we don't try to pull too many.
dispatch :: (KnownNat (n+1), KnownNat f, KnownNat s, KnownNat d)
         => (Op (StationID f s) -> Index f) 
         -> DistpatchState n f s d
         -> Either (DistpatchState n f s d) (DistpatchState n f s d)
dispatch select state@(DS insts regs stations rob) = case take insts of
    (_, Nothing)      -> Left state
    (insts', Just fetched@(Fetched pc pred (Ldr a b r))) -> 
        if thereIsAddSpace && thereIsLdSpace && thereIsRobSpace
            then Right $ DS insts' regs' stations' rob'
            else Left state
        where
        thereIsRobSpace = twoFree rob
        -- Assumption: Add stations and Ld stations are disjoint
        fakeAdd = copyFrom regs $ Add a b (error "Error: Virtual add register should not be used!")
        addFu = select fakeAdd
        addSlot = freeSlot stations addFu 
        thereIsAddSpace | (Just _) <- addSlot = True
                        | Nothing  <- addSlot = False
        addStation = StationID addFu (fromJust addSlot)
        -- Time to make the fake load now
        fakeLd = Ld (Pending addStation) r
        ldFu = select fakeLd
        ldSlot = freeSlot stations ldFu
        thereIsLdSpace | (Just _) <- ldSlot = True
                       | Nothing  <- ldSlot = False
        ldStation = StationID ldFu (fromJust ldSlot)
        regs' = renameReg r ldStation regs
        stations' = insert addStation fakeAdd $ insert ldStation fakeLd $ stations
        rob' = waitFor (Fetched pc (Predicted pc  ) fakeLd)  ldStation  $ 
               waitFor (Fetched pc pred             fakeAdd) addStation $ rob
    (insts', Just fetched@(Fetched pc pred op)) ->
        if thereIsSpace && thereIsRobSpace
            then Right $ DS insts' regs' stations' rob'
            else Left state
        where
        thereIsRobSpace = oneFree rob
        op' = copyFrom regs op
        fu = select op'
        slot = freeSlot stations fu
        thereIsSpace | (Just _) <- slot = True
                     | Nothing  <- slot = False
        opStation = StationID fu (fromJust slot)
        stations' = insert opStation op' stations
        regs' = case op' of
            Mov _   r -> overwrite r 
            Add _ _ r -> overwrite r
            Ld  _   r -> overwrite r
            Jeq _ _ _ -> regs
            Jmp _     -> regs
        overwrite r = renameReg r opStation regs
        rob' = waitFor (Fetched pc pred op') opStation rob








