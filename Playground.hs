{-# LANGUAGE ScopedTypeVariables #-}
module Playground (CPUState, Playground.empty, cpu) where

import CPU.Dispatch (DispatchState(DS), dispatchN, empty)
import CPU.OpBuffer (empty, insert, OpBuffer(fetch_pc))
import CPU.Op (Op(..), Fetched(..))
import CPU.RegisterFile (RegisterFile, empty)
import CPU.Defs (Predicted(..), RVal(..), MemRead(NothingRead, ReadSome), Read, RIx, Halt(..), PC, Fetch)
import CPU.ReorderBuffer (ROB, empty)
import CPU.RStations (RStations, empty)
import CPU.FunctionalUnits (FUStates, FUsC, select, empty, step)
import CPU.BackupRegs (BackupRegs, restore, empty)
import CPU.Commit (CommitState(CS), Action(Jump,Stop,OK), commitN)
import CPU.CDBBroadcast (CDBBS(..), process)
import CPU.Fetch (FetchState, empty, fetch)
import CLaSH.Prelude hiding (select, Read)
import Data.List (intercalate)
import Text.Printf (printf)

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
    fustates :: FUStates l l' f f' c c' rb,
    backups  :: BackupRegs,
    fetching :: FetchState
}

instance (KnownNat ob, KnownNat rb) => Show (CPUState l l' f f' c c' rh ob rb ds) where
    show (CPUState opBuffer regFile stations rob fustates backups fetching) = intercalate "\n\n" [
        printf "Op Buffer: %s" (show opBuffer),
        printf "Register File: %s" (show regFile),
        printf "Registration stations: %s" (show stations),
        printf "Reorder buffer: %s" (show rob),
        printf "Functional unit states: %s" (show fustates),
        printf "Register backups: %s" (show backups),
        printf "Fetch state: %s" (show fetching) ]

insertOp :: KnownNat ob => CPUState l l' f f' c c' rh ob rb ds -> Fetched (Op RIx) -> CPUState l l' f f' c c' rh ob rb ds
insertOp state op = state {opBuffer = CPU.OpBuffer.insert (opBuffer state) op}

s1 = insertOp Playground.empty $ Fetched 0 (Predicted 1) (Mov 3 4)

s2 = insertOp s1 $ Fetched 1 (Predicted 2) (Mov 7 8)

res = cpu s2 (repeat NothingRead)

prog = zipWith3 (\pc pred op -> Fetched pc (Predicted pred) op) (iterateI (+1) 0) (iterateI (+1) 1)

s3 = foldl insertOp Playground.empty' $ prog (Mov 3 1 :> Mov 5 1 :> Add (Pending 1) (Pending 1) 2 :> Nil)



empty' :: CPUState 3 4 3 4 2 6 12 16 64 6
empty' = Playground.empty

empty :: (FUsC l l' f f' c c' rh, KnownNat ob, KnownNat rb) => CPUState l l' f f' c c' rh ob rb ds
empty = CPUState CPU.OpBuffer.empty 
                 CPU.RegisterFile.empty 
                 CPU.RStations.empty 
                 CPU.ReorderBuffer.empty 
                 CPU.FunctionalUnits.empty 
                 CPU.BackupRegs.empty 
                 CPU.Fetch.empty

cpu :: forall ldus lduslots -- must mult to resheight
              fpus fpuslots -- must mult to resheight
              cus  cuslots  -- must mult to resheight
              resheight     -- Height of reservation stations (width is 3)
              opbuffer      -- Size of fetch buffer
              reorderbuffer -- Size of reorder buffer
              dispatches    -- Number of dispatches (and commits, but they could be differernt) per cycle
              obconstraint  -- This is just here to convince the compiler that the op buffer is non-empty
              rbconstraint . -- ditto ^
              (FUsC ldus lduslots fpus fpuslots cus cuslots resheight,
               KnownNat rbconstraint,
               KnownNat reorderbuffer,
               reorderbuffer ~ (rbconstraint + 1),
               KnownNat obconstraint,
               KnownNat opbuffer,
               opbuffer ~ (obconstraint + 1),
               KnownNat dispatches)
              => CPUState ldus lduslots fpus fpuslots cus cuslots resheight opbuffer reorderbuffer dispatches
              -> Vec ldus MemRead
              -> Vec dispatches MemRead
              -> (CPUState ldus lduslots fpus fpuslots cus cuslots resheight opbuffer reorderbuffer dispatches, Vec ldus Read, Vec dispatches Fetch, Halt)
cpu (CPUState opBuffer regFile stations rob fustates backups fetching) reads fetches = (state', reqs, fetchRequests, halt)
    where
    (fetching', opBuffer', fetchRequests) = fetch fetching opBuffer fetches
    dispatch0 = DS opBuffer' regFile stations rob
    dispatchn = dispatchN (snat :: SNat dispatches) select dispatch0
    (DS opBuffer'' regFile' stations' rob') = dispatchn
    (fustates', stations'', cdbmessages, reqs) = step fustates stations' reads
    broadcast0 = CDBBS regFile' stations'' rob'
    broadcast' = process cdbmessages broadcast0
    (CDBBS regFile'' stations''' rob'') = broadcast'
    commit0 = CS backups OK rob''
    commitn = commitN (snat :: SNat dispatches) commit0
    (CS backups' action rob''') = commitn
    halt = case action of
        Stop -> DoHalt
        _    -> DontHalt
    state' = case action of
        OK      -> CPUState opBuffer'' regFile'' stations''' rob''' fustates' backups' fetching'
        Jump pc -> reset pc backups'
        Stop    -> error "Trying to run after a halt!"

reset :: forall l l' f f' c c' rh ob rb ds . (FUsC l l' f f' c c' rh, KnownNat ob, KnownNat rb) 
      => PC -> BackupRegs -> CPUState l l' f f' c c' rh ob rb ds
reset pc backup = empty {opBuffer = with pc, regFile = restore backup}
    where
    with pc = CPU.OpBuffer.empty {fetch_pc = pc} :: OpBuffer ob
    empty = Playground.empty :: CPUState l l' f f' c c' rh ob rb ds


--topEntity :: CPUState 3 4 3 4 2 6 12 16 64 6 -> Vec 3 MemRead -> Vec 6 MemRead -> (CPUState 3 4 3 4 2 6 12 16 64 6, Vec 3 Read, Vec 6 Fetch, Halt)
--topEntity = cpu