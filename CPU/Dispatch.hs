{-# LANGUAGE ScopedTypeVariables #-}

module CPU.Dispatch (CPU.Dispatch.empty, dispatch, dispatchN, DispatchState(..)) where

import Text.Printf
import CLaSH.Prelude hiding (take, empty)
import Data.Maybe (fromJust)
import CPU.Defs (RVal(Pending, Literal), RIx, Predicted(..), StationID(..), RobID(..))
import CPU.Op (Fetched(..), Op(..))
import CPU.RegisterFile as RF (RegisterFile, renameReg, copyFrom, empty)
import CPU.OpBuffer as OB (OpBuffer, take, empty)
import CPU.RStations as RS (RStations, RSEntry(..), freeSlot, insert, empty)
import CPU.ReorderBuffer as ROB (ROB, oneFree, twoFree, robInsert, empty)

data DispatchState n f s r = DS {opBuffer :: (OpBuffer n),
                                 regFile  :: (RegisterFile r),
                                 stations :: (RStations f s r),
                                 rob      :: (ROB r) } deriving (Eq)

instance KN n f s r => Show (DispatchState n f s r) where
    show (DS ob rf rs rob) = printf "Dispatch:\n  %s\n  %s\n  %s\n  %s" (show ob) (show rf) (show rs) (show rob)

type KN n f s r = (KnownNat n, KnownNat f, KnownNat s, KnownNat r)

empty :: KN n f s r => DispatchState n f s r
empty = DS OB.empty RF.empty RS.empty ROB.empty


-- We return left if it failed, right if it succeeded. This way,
-- we can write a do-expression to update dispatch with multiple instructions
-- and we don't try to pull too many.
dispatch :: KN (n+1) f s r
         => (Op (RobID r) -> Index f) 
         -> DispatchState (n+1) f s r
         -> Either (DispatchState (n+1) f s r) (DispatchState (n+1) f s r)
dispatch select state@(DS insts regs stations rob) = case take insts of
    (_, Nothing)      -> Left state
    (insts', Just fetched@(Fetched pc pred (Ldr a b r))) -> 
        if thereIsAddSpace && thereIsLdSpace && thereIsRobSpace
            then Right $ DS insts' regs' stations' rob''
            else Left state
        where
        thereIsRobSpace = twoFree rob
        -- Assumption: Add stations and Ld stations are disjoint
        fakeAdd = copyFrom regs $ Add a b 0-- (error "Error: Virtual add register should not be used!")
        addFu = select fakeAdd
        addSlot = freeSlot stations addFu 
        thereIsAddSpace | (Just _) <- addSlot = True
                        | Nothing  <- addSlot = False
        addStation = StationID addFu (fromJust addSlot)
        -- Time to make the fake load now
        fakeLd = Ld (Pending addID) r
        ldFu = select fakeLd
        ldSlot = freeSlot stations ldFu
        thereIsLdSpace | (Just _) <- ldSlot = True
                       | Nothing  <- ldSlot = False
        ldStation = StationID ldFu (fromJust ldSlot)
        (addID, rob')  = robInsert (Fetched pc pred fakeAdd) rob
        (ldID,  rob'') = robInsert (Fetched pc pred fakeLd)  rob'
        regs' = renameReg r ldID regs
        stations' = insert addStation (RSEntry fakeAdd addID) 
                  $ insert ldStation  (RSEntry fakeLd ldID) 
                  $ stations

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
        (opID, rob') = robInsert (Fetched pc pred op') rob
        stations' = insert opStation (RSEntry op' opID) stations
        regs' = case op' of
            Mov _   r -> overwrite r 
            Add _ _ r -> overwrite r
            Ld  _   r -> overwrite r
            Jeq _ _ _ -> regs
            Jmp _     -> regs
            Halt      -> regs
            _         -> error "Unknown op in dispatch" -- (printf "Unknown op in dispatch: %s" (show op'))
        overwrite r = renameReg r opID regs

-- Dispatch up to n times, as many as we can fit
dispatchN :: forall m n f s r . KN (m+1) f s r
         => SNat n
         -> (Op (RobID r) -> Index f) 
         -> DispatchState (m+1) f s r
         -> DispatchState (m+1) f s r
dispatchN n select state = either id id $ goN (return state)
    where
    dispatch' () state = state >>= dispatch select
    goN state = foldr dispatch' state (replicate n ())
