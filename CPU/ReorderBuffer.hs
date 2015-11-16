module CPU.ReorderBuffer (ROB(..), Waiting(..), oneFree, twoFree, robInsert, empty) where

import CLaSH.Prelude hiding (empty)
import CPU.Defs (StationID, RobID(..))
import CPU.Op (Op, Fetched)
import CPU.Buffer (Buffer, intStats, full, insert')
import qualified CPU.Buffer as Buf

data ROB r = ROB {buf   :: (Buffer r (Waiting r)),
                  first :: (RobID r),
                  next  :: (RobID r)} deriving (Show, Eq)

data Waiting r = Waiting (Fetched (Op (RobID r))) 
               | Done    (Fetched (Op (RobID r))) 
               deriving (Show, Eq)

empty :: KnownNat r => ROB r
empty = ROB Buf.empty (RobID 0) (RobID 0)

oneFree :: KnownNat r => ROB r -> Bool
oneFree (ROB buf _ _) = not (full buf)

twoFree :: KnownNat r => ROB r -> Bool
twoFree (ROB buf _ _) = let (max, count) = intStats buf in max - count >= 2

robInsert :: KnownNat r => Fetched (Op (RobID r)) -> ROB r -> (RobID r, ROB r)
robInsert op (ROB buf first next) = (next, ROB (insert' buf $ Waiting op) first (succ next))

robPop :: KnownNat (r+1) => ROB (r+1) -> (Maybe (Fetched (Op (RobID (r+1)))), ROB (r+1))
robPop rob@(ROB buf first next) = case take buf of
    (buf,  Nothing)    -> (Nothing, rob)
    (buf', Just entry) -> case entry of
        Waiting _ -> (Nothing, rob)
        Done op   -> (Just op, ROB buf' (succ first) next)



--waitFor :: KnownNat n => Fetched (Op (StationID f s)) -> StationID f s -> ROB n f s -> ROB n f s
--waitFor op from (ROB buf) = ROB $ insert' buf $ Waiting op from