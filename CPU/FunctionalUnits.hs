{-# LANGUAGE ScopedTypeVariables #-}

module CPU.FunctionalUnits (FUStates, FUsC, select, step, topEntity, empty) where

import CLaSH.Prelude hiding (select, split, Read, empty)
import CPU.Defs (RobID, RVal(..), Read(..), MemRead(..), RIx, W(w), Addr(Addr))
import CPU.Op (Op(..), grounded)
import CPU.CDBMessage (CDBMessage(..), CDBData(..))
import Data.Maybe (isNothing)
import CPU.RStations (RSEntry(..), RStations(..))

type N n = KnownNat n

select :: N r => Op (RobID r) -> Index 3
select op = case op of
    Mov _ _   -> 0
    Add _ _ _ -> 0
    Ld  _ _   -> 1
    Jmp _     -> 2
    Jeq _ _ _ -> 2
    Halt      -> 2

split :: (N n, N m) => Vec (n*m) a -> Vec n (Vec m a)
split = transpose . unconcatI

tilps :: (N n, N m) => Vec n (Vec m a) -> Vec (n*m) a
tilps = concat . transpose

data FUStates (mem  :: Nat) (mem'  :: Nat) 
              (fp   :: Nat) (fp'   :: Nat)
              (ctrl :: Nat) (ctrl' :: Nat)
              (r    :: Nat) = FUStates (Vec mem (LDUState r)) (Vec fp FPUState) (Vec ctrl CUState) deriving Show

empty :: FUsC m m' f f' c c' y => FUStates m m' f f' c c' r
empty = FUStates (repeat initialMem) (repeat initialFP) (repeat initialCtrl)

-- Constraints on the functional unit parameters
type FUsC m m' f f' c c' y = (N m, N m',    -- Number of mem FUs, slots per mem FU
                              N f, N f',    -- ""
                              N c, N c',    -- ""
                              N y,          -- # of registration slots per FU type
                              (m * m') ~ y, -- # of mem FUs * slots per mem FU must == total slots
                              (f * f') ~ y, -- ""
                              (c * c') ~ y) -- ""

-- The type of a registration station column that has been split up
type Splitted n n' r = Vec n (Vec n' (Maybe (RSEntry r)))

step :: forall m m' f f' c c' y r . (FUsC m m' f f' c c' y, N r) 
     => FUStates m m' f f' c c' r
     -> RStations 3 y r 
     -> Vec m MemRead
     -> (FUStates m m' f f' c c' r, RStations 3 y r, Vec (m + f + c) (Maybe (CDBMessage r)), Vec m Read)
step (FUStates memStates fpStates ctrlStates) (RStations (fps :> mems :> ctrls :> Nil)) reads = 
    (fustates', rstations', msgs, reqs)
    where
    fustates'  = FUStates memStates' fpStates' ctrlStates' 
    rstations' = RStations (fps''' :> mems''':> ctrls''' :> Nil)
    mems'  = split mems  :: Splitted m m' r
    fps'   = split fps   :: Splitted f f' r
    ctrls' = split ctrls :: Splitted c c' r
    (memStates',  mems'',  memmsgs, reqs) = unzip4 $ zipWith3 stepMem reads mems'  memStates
    (fpStates',   fps'',   fpmsgs)        = unzip3 $ zipWith  stepFP        fps'   fpStates
    (ctrlStates', ctrls'', ctrlmsgs)      = unzip3 $ zipWith  stepCtrl      ctrls' ctrlStates
    mems'''  = tilps mems''
    fps'''   = tilps fps''
    ctrls''' = tilps ctrls''
    msgs = memmsgs ++ fpmsgs ++ ctrlmsgs

unzip4 :: Vec n (a,b,c,d) -> (Vec n a, Vec n b, Vec n c, Vec n d)
unzip4 xs = ( map (\(w,_,_,_) -> w) xs
            , map (\(_,x,_,_) -> x) xs
            , map (\(_,_,y,_) -> y) xs
            , map (\(_,_,_,z) -> z) xs
            )

data LDUState r = Loading RIx (RobID r) | Empty deriving Show
stepMem :: N n => MemRead -> Vec n (Maybe (RSEntry r)) -> LDUState r -> (LDUState r, Vec n (Maybe (RSEntry r)), Maybe (CDBMessage r), Read)
stepMem read instrs (Loading rix rob) = case read of
    NothingRead -> (Loading rix rob, instrs, Nothing, NoRead)
    ReadSome x  -> case take1 instrs of
        (Nothing,    instrs') -> (Empty, instrs', cdbmessage x, NoRead)
        (Just (RSEntry instr rob), instrs') -> case instr of
            Ld (Literal addr) rix -> (Loading rix rob, instrs', cdbmessage x , Read $ Addr $ w $ addr) -- Only read at transition. It resets memory thing
            _                     -> error "Invalid instruction moved into LDU"
    where
    cdbmessage x = Just (CDBMessage rob (RegWrite x rix))
stepMem read instrs Empty = case read of
    -- ReadSome _  -> error "A memory read occurred when the LDU was not expecting it."
    -- NothingRead -> case take1 instrs of
    _ -> case take1 instrs of
        (Nothing,    instrs') -> (Empty, instrs', Nothing, NoRead)
        (Just (RSEntry instr rob), instrs') -> case instr of
            Ld (Literal addr) rix -> (Loading rix rob, instrs', Nothing, Read $ Addr $ w $ addr) -- Only read at transition. It resets memory thing
            _                     -> error "Invalid instruction moved into LDU"

data FPUState = FPUState deriving Show
stepFP :: N n => Vec n (Maybe (RSEntry r)) -> FPUState -> (FPUState, Vec n (Maybe (RSEntry r)), Maybe (CDBMessage r))
stepFP instrs FPUState = case take1 instrs of
    (Nothing, instrs') -> (FPUState, instrs', Nothing)
    (Just (RSEntry instr rob), instrs') -> case instr of
        Mov w rix                       -> (,,) FPUState instrs' $ Just $ CDBMessage rob (RegWrite w     rix)
        Add (Literal a) (Literal b) rix -> (,,) FPUState instrs' $ Just $ CDBMessage rob (RegWrite (a+b) rix)
        _ -> error "Invalid instruction moved into FPU"

data CUState = CUState deriving Show
stepCtrl :: N n => Vec n (Maybe (RSEntry r)) -> CUState -> (CUState,  Vec n (Maybe (RSEntry r)), Maybe (CDBMessage r))
stepCtrl instrs CUState = case take1 instrs of
    (Nothing, instrs') -> (CUState, instrs', Nothing)
    (Just (RSEntry instr rob), instr') -> (,,) CUState instr' $ Just $ case instr of
        Halt   -> CDBMessage rob $ DoHalt
        Jmp pc -> CDBMessage rob $ JumpTaken pc
        Jeq (Literal a) (Literal b) pc -> CDBMessage rob $ if a == b
            then JumpTaken pc
            else JumpNotTaken
        _      -> error "Invalid instruction moved into CU"

initialMem :: LDUState r
initialMem = Empty
initialFP :: FPUState
initialFP = FPUState
initialCtrl :: CUState
initialCtrl = CUState

take1 :: N n => Vec n (Maybe (RSEntry r)) -> (Maybe (RSEntry r), Vec n (Maybe (RSEntry r)))
take1 vec = case findIndex ready vec of
    Nothing -> (Nothing,   vec)
    Just ix -> (vec !! ix, replace ix Nothing vec)
    where 
    ready Nothing = False
    ready (Just (RSEntry instr _)) = grounded instr

-- Not so bad!
topEntity :: FUStates 3 4 3 4 2 6 64
          -> RStations 3 12 64
          -> Vec 3 MemRead
          -> (FUStates 3 4 3 4 2 6 64, RStations 3 12 64, Vec (3 + 3 + 2) (Maybe (CDBMessage 64)), Vec 3 Read)
topEntity = step