module CPU.CDBBroadcast (CDBBS(..), process) where 

import CLaSH.Prelude 
import CPU.CDBMessage (CDBMessage(..), CDBData(..))
import CPU.Op (Op(..), Fetched(..))
import CPU.Defs (RVal(..), W, RobID(..), RIx(..))
import CPU.RStations (RSEntry(..), RStations(..))
import CPU.RegisterFile (RegisterFile(..))
import CPU.ReorderBuffer (ROB(..), Waiting(..))
import CPU.Buffer (Buffer(..), Count(..))
import Text.Printf (printf)

data CDBBS x y r = CDBBS (RegisterFile r) (RStations x y r) (ROB r)

process :: KnownNat r => Vec n (Maybe (CDBMessage r)) -> CDBBS x y r -> CDBBS x y r
process messages (CDBBS (RegFile regs) (RStations stations) rob) 
    = CDBBS (RegFile regs') (RStations stations') rob'
    where
    regs' = map (updateRegWith messages) $ zip regs $ map RIx indicesI
    stations' = map (map (fmap (updateStationWith messages))) stations
    rob' = foldr updateRobWith rob messages

updateStationWith :: Vec n (Maybe (CDBMessage r)) -> RSEntry r -> RSEntry r
updateStationWith messages entry = foldr updateStationWith' entry messages

updateStationWith' :: Maybe (CDBMessage r) -> RSEntry r -> RSEntry r
updateStationWith' (Just (CDBMessage rob (RegWrite w _))) (RSEntry op x) = RSEntry op' x
    where op' = replaceOperands rob w op
updateStationWith' _ entry = entry

replaceOperands :: RobID r -> W -> Op (RobID r) -> Op (RobID r)
replaceOperands rob w op = case op of
    Add a b r -> Add (rep a) (rep b) r
    Ld a r    -> Ld  (rep a)         r
    Jeq a b r -> Jeq (rep a) (rep b) r
    otherOp -> otherOp
    where
    rep (Literal w)    = Literal w
    rep (Pending rob') = if rob == rob'
        then Literal w
        else Pending rob'

updateRegWith :: Vec n (Maybe (CDBMessage r)) -> (RVal (RobID r), RIx) -> RVal (RobID r)
updateRegWith _        (Literal w,   _)   = Literal w
updateRegWith messages (Pending rob, rix) = case checked of
        Nothing -> Pending rob
        Just w  -> Literal w
    where
    checked = foldr (<|>) Nothing (map pertinent messages)
    pertinent (Just (CDBMessage rob' (RegWrite w rix'))) | rob == rob' = if rix == rix'
            then Just w
            else error "ROB match, RIx mismatch"
    pertinent _ = Nothing


updateRobWith :: KnownNat r => Maybe (CDBMessage r) -> ROB r -> ROB r
updateRobWith Nothing rob = rob
updateRobWith (Just (CDBMessage (RobID rob) msg)) (ROB buf (RobID first) next) 
    = ROB buf' (RobID first) next
    where
    index = if rob >= first
        then rob - first
        else cyclicNegate (first - rob)
    cyclicNegate x = (maxBound - x) + 1
    (Buffer count items) = buf
    invalid = (Count index) > count
    item | invalid   = error "ROB update is trying to access a non-existent element"
         | otherwise = items !! index
    buf' = Buffer count $ replace index (updateRobEntry item msg) items

-- NB: We don't actually care about the arguments in the ROB.
-- Expected output instructions:
-- Mov, Add, Ld -> Mov
-- Halt -> Halt
-- Jmp, Jeq -> Jmp
updateRobEntry :: Waiting r -> CDBData -> Waiting r
updateRobEntry (Done _)     _       = error "Trying to update ROB entry for an instruction that's done"
updateRobEntry (Waiting fetched@(Fetched pc pred op)) message = case (op, message) of
        (Mov w rix, RegWrite w' rix')  | w /= w'     -> error "ROB Mov and CDB Mov disagree on w"
                                       | rix /= rix' -> error "ROB Mov and CDB Mov disagree on rix"
                                       | otherwise   -> Done fetched
        (Add _ _ rix, RegWrite w rix') | rix /= rix' -> error "ROB Add and CDB Add disagree on rix"
                                       | otherwise   -> Done (Fetched pc pred (Mov w rix))
        (Jmp pc',      JumpTaken pc'') | pc' /= pc'' -> error "ROB Jmp and CDB Jmp disagree on pc'"
                                       | otherwise   -> Done fetched
        (Halt,        DoHalt)                        -> Done fetched
        (Ld _ rix,    RegWrite w rix') | rix /= rix' -> error "ROB Ld and CDB Ld disagree on rix"
                                       | otherwise   -> Done (Fetched pc pred (Mov w rix))
        (Jeq _ _ pc', JumpTaken pc'')  | pc' /= pc'' -> error "ROB Jeq and CDB Jeq disagree on pc'" -- $ printf "ROB Jeq and CDB Jeq disagree on pc'. fetched: %s msg: %s" (show fetched) (show message)
                                       | otherwise   -> Done (Fetched pc pred (Jmp pc'))
        (Jeq _ _ _,   JumpNotTaken)                  -> Done (Fetched pc pred (Jmp (pc + 1)))
        (_,           _)                             -> error "Unexpected CDB message received in ROB"


-- There are n elements in the buffer.
-- The ROB indices of the elements are a 
--    linearly increasing cyclic group.
-- How many elements there are in the buffer
--    has nothing to do with whether it cycles.
-- The linear index of the element we're looking for
--    is ix - first.
-- Let's say we're in Z/7.
-- Useful: (-n) = (maxVal+1) - n = (maxVal - n) + 1
-- Also : (a - b) = - (b - a)
-- 4 - 2 = 2
-- 5 - 6 = -1 = 0 - 1 = 7 - 1 = (6-1)+1 = 6
-- 3 - 6 = -3 = 0 - 3 = 7 - 3 = (6-3)+1 = 4

