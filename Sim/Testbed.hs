module Sim.Testbed (loadHexFile, loadHexFile', withRAM) where

import CLaSH.Prelude 

import Hardware (cpu') 

import Sim.Mem (mem)

import Numeric (readHex)

import CPU.BackupRegs (BackupRegs)

import CPU.Defs (W(..), Halt)

import Text.Printf (printf)

import qualified Prelude as P


loadHexFile' :: String -> IO [W]
loadHexFile' path = do
    hexes <- lines <$> readFile path
    let list = P.map (fst . P.head . readHex) hexes
    return list

loadHexFile :: KnownNat n => String -> IO (Vec n W)
loadHexFile path = do
    hexes <- lines <$> readFile path
    let list = P.map (fst . P.head . readHex) hexes
    let vec = map P.head $ iterateI P.tail list
    return vec

withRAM :: Vec 65536 W -> Signal String
withRAM ram = output
    where
    (loads, fetches) = unbundle $ mem 100 1 ram loadReqs fetchReqs
    (loadReqs, fetchReqs, halt, backups, cpustate) = unbundle $ cpu' loads fetches
    output = format <$> halt <*> backups <*> cpustate
    format halt backups cpustate = printf "--------------------\nHalting: %s\nBackups: %s\nState:%s\n\n\n" (show halt) (show backups) (show cpustate)


