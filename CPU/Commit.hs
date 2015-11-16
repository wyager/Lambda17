module CPU.Commit (Action(..), CommitState(..), commitN) where

import CLaSH.Prelude
import CPU.ReorderBuffer (ROB, robPop)
import CPU.Op (Fetched(..), Op(Mov,Jmp,Halt)) -- Theoretically we should only see these 3
import CPU.BackupRegs (BackupRegs, set)
import CPU.Defs (PC, Predicted(..))


data Action = Jump PC | Stop | OK deriving (Show, Eq)

data CommitState r = CS BackupRegs Action (ROB r) deriving (Show, Eq)

commit1 :: KnownNat (r+1) => CommitState (r+1) -> CommitState (r+1)
commit1 old@(CS backup Stop     rob) = old -- We are halting. Stop
commit1 old@(CS backup (Jump _) rob) = old -- We are jumping. Stop
commit1 old@(CS backup OK       rob) = case robPop rob of
        (Nothing, _) -> old -- There are no instructions ready to go. Do nothing.
        (Just (Fetched pc pred op), rob') -> CS written action rob'
            where
            written = case op of
                Mov w r -> set r w backup
                Jmp _   -> backup
                Halt    -> backup
                _       -> error "Unexpected instruction in ROB commit"
            action = case op of
                Halt    -> Stop
                Jmp pc' -> if (Predicted pc') == pred
                    then OK
                    else Jump pc'
                Mov _ _ -> OK
                _       -> error "Unexpected instruction in ROB commit"

commitN :: KnownNat (r+1) => SNat n -> CommitState (r+1) -> CommitState (r+1)
commitN n cs = foldr commit1' cs (replicate n ())
    where
    commit1' () cs = commit1 cs

{-
I need to pop things off the ROB.
As I pop things off the ROB, I need to check
if there was a mispredict. If there was, stop popping
things into the commit state.
-}