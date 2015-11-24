module Hardware (topEntity, cpu') where

import CLaSH.Prelude hiding (empty, Read)
import CPU.Defs (Read(..), MemRead(..), Fetch(..), Halt(..), Addr(..), PC(..), W(..))
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

initial :: CPUState LDUs LDUSlots FPUs FPUSlots CUs CUSlots Height IBuf RBuf DispatchPerCycle
initial = Playground.empty

cpu' :: Signal (Vec LDUs MemRead) -> Signal (Vec DispatchPerCycle MemRead) -> Signal (Vec LDUs Read, Vec DispatchPerCycle Fetch, Halt, BackupRegs, CPUState LDUs LDUSlots FPUs FPUSlots CUs CUSlots Height IBuf RBuf DispatchPerCycle)
cpu' loads fetches = bundle (loadReqs, fetchReqs, halt, backup, state')
    where
    cpustate = register initial state'
    (state', loadReqs, fetchReqs, halt, backup) = unbundle $ cpu <$> cpustate <*> loads <*> fetches


topEntity :: Signal (Vec LDUs Bit) 
          -> Signal (Vec LDUs (BitVector 16)) 
          -> Signal (Vec DispatchPerCycle Bit)
          -> Signal (Vec DispatchPerCycle (BitVector 16)) 
          -> Signal (Vec LDUs Bit, Vec LDUs (BitVector 16), Vec DispatchPerCycle Bit, Vec DispatchPerCycle (BitVector 16), Bool, Vec 16 (BitVector 16)) 
topEntity loadEns loads fetchEns fetches = bundle (loadReqEn, loadReqBits, fetchReqEn, fetchReqBits, halt', backups')
    where
    mkMemRead :: Bit -> BitVector 16 -> MemRead
    mkMemRead 1 w = ReadSome (W w)
    mkMemRead 0 _ = NothingRead
    loads' = zipWith mkMemRead <$> loadEns <*> loads :: Signal (Vec LDUs MemRead)
    fetches' = zipWith mkMemRead <$> fetchEns <*> fetches :: Signal (Vec DispatchPerCycle MemRead)
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