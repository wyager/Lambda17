{-# LANGUAGE FlexibleContexts #-}
module Hardware (topEntity, cpu') where

import Clash.Prelude hiding (empty, Read)
import CPU.Defs (S, Clk, Read(..), MemRead(..), Fetch(..), Halt(..), Addr(..), PC(..), W(..))
import Playground (CPUState, empty, cpu)
import CPU.BackupRegs (BackupRegs(..))

-- CPU parameters
type LDUs = 3
type LDUSlots = 2
type FPUs = 2
type FPUSlots = 3
type CUs = 1
type CUSlots = 6
type Height = 6
type IBuf = 16
type RBuf = 16
type DispatchPerCycle = 2

type State = CPUState LDUs LDUSlots FPUs FPUSlots CUs CUSlots Height IBuf RBuf DispatchPerCycle

initial :: State
initial = Playground.empty

cpu' :: Clk
     => S (Vec LDUs MemRead)
     -> S (Vec DispatchPerCycle MemRead)
     -> S (Vec LDUs Read, Vec DispatchPerCycle Fetch, Halt, BackupRegs, State)
cpu' loads fetches = bundle (loadReqs, fetchReqs, halt, backup, state')
    where
    cpustate = register initial state'
    (state', loadReqs, fetchReqs, halt, backup) = unbundle $ cpu <$> cpustate <*> loads <*> fetches

type Out = ( Vec LDUs Bit, Vec LDUs (BitVector 16)
           , Vec DispatchPerCycle Bit, Vec DispatchPerCycle (BitVector 16)
           , Bool, Vec 16 (BitVector 16) )

-- Factored out so topEntity's withClockResetEnable scope encloses the
-- call to cpu' (which needs HiddenClockResetEnable).
adapter :: Clk
        => S (Vec LDUs Bit)
        -> S (Vec LDUs (BitVector 16))
        -> S (Vec DispatchPerCycle Bit)
        -> S (Vec DispatchPerCycle (BitVector 16))
        -> S Out
adapter loadEns loads fetchEns fetches =
    bundle (loadReqEn, loadReqBits, fetchReqEn, fetchReqBits, haltL, backupsL)
    where
    mkMemRead :: Bit -> BitVector 16 -> MemRead
    mkMemRead 1 w = ReadSome (W w)
    mkMemRead _ _ = NothingRead
    loads'   = zipWith mkMemRead <$> loadEns  <*> loads   :: S (Vec LDUs MemRead)
    fetches' = zipWith mkMemRead <$> fetchEns <*> fetches :: S (Vec DispatchPerCycle MemRead)
    (loadReqs, fetchReqs, halt, backups, _) = unbundle $ cpu' loads' fetches'
    (loadReqEn, loadReqBits) = unbundle $ (unzip . map unMkLoadReq) <$> loadReqs
    unMkLoadReq :: Read -> (Bit, BitVector 16)
    unMkLoadReq NoRead = (0,0)
    unMkLoadReq (Read (Addr w)) = (1,w)
    (fetchReqEn, fetchReqBits) = unbundle $ (unzip . map unMkFetchReq) <$> fetchReqs
    unMkFetchReq :: Fetch -> (Bit, BitVector 16)
    unMkFetchReq NoFetch = (0,0)
    unMkFetchReq (Fetch (PC pc)) = (1,pc)
    halt' = halt2Bits <$> halt
    halt2Bits DoHalt = True
    halt2Bits DontHalt = False
    backups' = (\(BackupRegs regs) -> map (\(W x) -> x) regs) <$> backups
    -- Latch halt; freeze backup snapshot once halted. cpu keeps running
    -- after Halt (Playground.cpu's state' is `error` on Stop, and
    -- commitN restarts with OK each cycle) so backups' is undefined
    -- on post-Halt cycles.
    haltL = register False (haltL .||. halt')
    backupsL = register (repeat 0) (mux haltL backupsL backups')

topEntity :: Clock System -> Reset System
          -> S (Vec LDUs Bit)
          -> S (Vec LDUs (BitVector 16))
          -> S (Vec DispatchPerCycle Bit)
          -> S (Vec DispatchPerCycle (BitVector 16))
          -> S Out
topEntity clk rst a b c d =
    withClockResetEnable clk rst enableGen (adapter a b c d)
{-# ANN topEntity
  (Synthesize
    { t_name   = "lambda17"
    , t_inputs = [ PortName "clk", PortName "rst"
                 , PortName "loadEns", PortName "loads"
                 , PortName "fetchEns", PortName "fetches" ]
    , t_output = PortName "out"
    }) #-}
{-# NOINLINE topEntity #-}