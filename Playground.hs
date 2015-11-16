{-# LANGUAGE ScopedTypeVariables #-}
module Playground () where

import CPU.Dispatch (DispatchState(DS), dispatchN, empty)
import CPU.OpBuffer (empty, insert, OpBuffer)
import CPU.Op (Op(..), Fetched(..))
import CPU.RegisterFile (RegisterFile, empty)
import CPU.Defs (Predicted(..), RVal(..), MemRead(NothingRead, ReadSome), Read, RIx)
import CPU.ReorderBuffer (ROB, empty)
import CPU.RStations (RStations, empty)
import CPU.FunctionalUnits (FUStates, FUsC, select, empty, step)
import CLaSH.Prelude hiding (select, Read)

--s0 = CPU.Dispatch.empty :: DispatchState 5 3 2 10

--insertOp s o = s {CPU.OpBuffer.opBuffer = CPU.OpBuffer.insert (opBuffer s) o}

--s1 = insertOp s0 (Fetched 0 (Predicted 1) (Mov 5 5))

--s2 = insertOp s1 (Fetched 1 (Predicted 2) (Ldr (Pending 5) (Pending 5) 7))

data CPUState (l  :: Nat)
              (l' :: Nat)
              (f  :: Nat)
              (f' :: Nat)
              (c  :: Nat)
              (c' :: Nat)
              (rh :: Nat)
              (ob :: Nat)
              (rb :: Nat)
              (ds :: Nat)
              = CPUState {
    opBuffer :: OpBuffer ob,
    regFile  :: RegisterFile rb,
    stations :: RStations 3 rh rb,
    rob      :: ROB rb,
    fustates :: FUStates l l' f f' c c' rb
} deriving Show

insertOp :: KnownNat ob => CPUState l l' f f' c c' rh ob rb ds -> Fetched (Op RIx) -> CPUState l l' f f' c c' rh ob rb ds
insertOp state op = state {opBuffer = CPU.OpBuffer.insert (opBuffer state) op}

s1 = insertOp Playground.empty $ Fetched 0 (Predicted 1) (Mov 3 4)

s2 = insertOp s1 $ Fetched 1 (Predicted 2) (Mov 7 8)

res = cpu s2 (repeat NothingRead)


empty :: CPUState 3 4 3 4 2 6 12 16 64 2
empty = CPUState CPU.OpBuffer.empty CPU.RegisterFile.empty CPU.RStations.empty CPU.ReorderBuffer.empty CPU.FunctionalUnits.empty

cpu :: forall ldus lduslots -- must mult to resheight
              fpus fpuslots -- must mult to resheight
              cus  cuslots  -- must mult to resheight
              resheight
              opbuffer
              reorderbuffer
              dispatches 
              obm . 
              (FUsC ldus lduslots fpus fpuslots cus cuslots resheight,
               KnownNat reorderbuffer,
               KnownNat obm,
               KnownNat opbuffer,
               opbuffer ~ (obm + 1),
               KnownNat dispatches)
              => CPUState ldus lduslots fpus fpuslots cus cuslots resheight opbuffer reorderbuffer dispatches
              -> Vec ldus MemRead
              -> (CPUState ldus lduslots fpus fpuslots cus cuslots resheight opbuffer reorderbuffer dispatches, Vec ldus Read)
cpu (CPUState opBuffer regFile stations rob fustates) reads = (CPUState opBuffer' regFile' stations'' rob' fustates', reqs)
    where
    dispatch0 = DS opBuffer regFile stations rob
    dispatchn = dispatchN (snat :: SNat dispatches) select dispatch0
    (DS opBuffer' regFile' stations' rob') = dispatchn
    (fustates', stations'', cdbmessages, reqs) = step fustates stations' reads

topEntity :: CPUState 3 4 3 4 2 6 12 16 64 2 -> Vec 3 MemRead -> (CPUState 3 4 3 4 2 6 12 16 64 2, Vec 3 Read)
topEntity = cpu