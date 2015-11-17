module Hardware (topEntity) where

import CLaSH.Prelude hiding (empty, Read)
import CPU.Defs (Read(..), MemRead(..), Fetch(..), Halt(..), Addr(..), PC(..), W(..))
import Playground (CPUState, empty, cpu)

initial :: CPUState 3 4 3 4 2 6 12 16 64 6
initial = Playground.empty

cpu' :: Signal (Vec 3 MemRead) -> Signal (Vec 6 MemRead) -> Signal (Vec 3 Read, Vec 6 Fetch, Halt)
cpu' loads fetches = bundle (loadReqs, fetchReqs, halt)
    where
    cpustate = register initial state'
    (state', loadReqs, fetchReqs, halt) = unbundle $ cpu <$> cpustate <*> loads <*> fetches


topEntity :: Signal (Vec 3 Bit) 
          -> Signal (Vec 3 (BitVector 16)) 
          -> Signal (Vec 6 Bit)
          -> Signal (Vec 6 (BitVector 16)) 
          -> Signal (Vec 3 Bit, Vec 3 (BitVector 16), Vec 6 Bit, Vec 6 (BitVector 16), Bool) 
topEntity loadEns loads fetchEns fetches = bundle (loadReqEn, loadReqBits, fetchReqEn, fetchReqBits, halt')
    where
    mkMemRead :: Bit -> BitVector 16 -> MemRead
    mkMemRead 1 w = ReadSome (W w)
    mkMemRead 0 _ = NothingRead
    loads' = zipWith mkMemRead <$> loadEns <*> loads :: Signal (Vec 3 MemRead)
    fetches' = zipWith mkMemRead <$> fetchEns <*> fetches :: Signal (Vec 6 MemRead)
    (loadReqs, fetchReqs, halt) = unbundle $ cpu' loads' fetches'
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