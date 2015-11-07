module CPU.ReorderBuffer (ROB, oneFree, twoFree, waitFor) where

import CLaSH.Prelude
import CPU.Defs (StationID)
import CPU.Op (Op, Fetched)
import CPU.Buffer (Buffer, intStats, full, insert')

data ROB n f s = ROB (Buffer n (Waiting f s))

data Waiting f s = Waiting (Fetched (Op (StationID f s))) (StationID f s) Bool

oneFree :: KnownNat n => ROB n f s -> Bool
oneFree (ROB buf) = not (full buf)

twoFree :: KnownNat n => ROB n f s -> Bool
twoFree (ROB buf) = let (max, count) = intStats buf in max - count >= 2

waitFor :: KnownNat n => Fetched (Op (StationID f s)) -> StationID f s -> ROB n f s -> ROB n f s
waitFor op from (ROB buf) = ROB $ insert' buf $ Waiting op from False