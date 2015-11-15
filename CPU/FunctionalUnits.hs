{-# LANGUAGE ScopedTypeVariables #-}

module CPU.FunctionalUnits (select) where

import CLaSH.Prelude hiding (select)
import CPU.Defs (RobID)
import CPU.Op (Op(..))
import CPU.CDBMessage (CDBMessage(..), CDBData(..))
import Data.Maybe (isNothing)

select :: KnownNat r => Op (RobID r) -> Index 3
select op = case op of
    Mov _ _   -> 0
    Add _ _ _ -> 0
    Ld  _ _   -> 1
    Jmp _     -> 2
    Jeq _ _ _ -> 2
    Halt      -> 2

split :: (KnownNat n, KnownNat m) => Vec (n*m) a -> Vec n (Vec m a)
split = transpose . unconcatI

tilps :: (KnownNat n, KnownNat m) => Vec n (Vec m a) -> Vec (n*m) a
tilps = concat . transpose

data FUStates mem mem' fp fp' ctrl ctrl' = FUStates (Vec mem LDUState) (Vec fp FPUState) (Vec ctrl CUState)

initialFUStates :: FUStates m m' f f' c c' 
initialFUStates = FUStates (repeat initial) (repeat initial) (repeat initial)

-- Constraints on the functional unit parameters
type FUsC m m' f f' c c' y = (KnownNat m, KnownNat m', -- Number of mem FUs, slots per mem FU
                              KnownNat f, KnownNat f', -- ""
                              KnownNat c, KnownNat c', -- ""
                              KnownNat y,              -- # of registration slots per FU type
                              (m * m') ~ y,            -- # of mem FUs * slots per mem FU must == total slots
                              (f * f') ~ y,            -- ""
                              (c * c') ~ y)            -- ""

-- The type of a registration station column that has been split up
type Splitted n n' r = Vec n (Vec n' (Maybe (RSEntry r)))

step :: forall m m' f f' c c' y r . (FUsC m m' f f' c c' y, KnownNat r) 
     => FUStates m m' f f' c c' -> RStations 3 y r -> (FUStates m m' f f' c c', RStations 3 y r, Vec (m + f + c) (Maybe CDBMessage))
step (FUStates memStates fpStates ctrlStates) (RStations (mems :> fps :> ctrls :> Nil)) = 
    (fustates', rstations', msgs)
    where
    fustates'  = FUStates memStates' fpStates' ctrlStates' 
    rstations' = RStations (mems''' :> fps''' :> ctrls''' :> Nil)
    mems'  = split mems  :: Splitted m m' r
    fps'   = split fps   :: Splitted f f' r
    ctrls' = split ctrls :: Splitted c c' r
    (memStates',  mems'',  memmsgs)  = unzip $ zipWith step' mems'  memStates
    (fpStates',   fps'',   fpmsgs)   = unzip $ zipWith step' fps'   fpStates
    (ctrlStates', ctrls'', ctrlmsgs) = unzip $ zipWith step' ctrls' ctrlStates
    mems'''  = tilps mems''
    fps'''   = tilps fps''
    ctrls''' = tilps ctrls''
    msgs = memmsgs ++ fpmsgs ++ ctrlmsgs


class FUState state where
    step' :: KnownNat n => Vec n (Maybe (RSEntry rob)) -> state -> (state, Vec n (Maybe (RSEntry rob)), Maybe CDBMessage)
    initial :: state

take1 :: Vec n (Maybe a) -> (Maybe a, Vec n (Maybe a))
take1 vec = case findIndex (not . isNothing) vec of
    Nothing -> (Nothing,   vec)
    Just ix -> (vec !! ix, replace ix Nothing vec)

data FPUState = FPUState

instance FUState FPUState where
    step' ops FPUState = (FPUState, ops', msg)
        where
        

data LDUState = LDUState Read -- Pending read

--instance FUState LDUState where
--    ready (LDUState NoRead) = True
--    ready (LDUState Read)   = False

data CUState = CUState

--instance FUState CUState where
--    ready = True

fpu :: Maybe (RSEntry rob)
    -> FPUState 
    -> (FPUState, Maybe CDBMessage)
fpu Nothing                 s = (,) s Nothing
fpu (Just (RSEntry op rob)) s = (,) s $ Just $ CDBMessage rob $ case op of
    Mov w rix -> RegWrite w rix
    Add a b r -> case (a,b) of
        (Literal aw, Literal bw) -> RegWrite (aw + bw) r
        _                        -> error "Ungrounded add instruction"
    _         -> error "Invalid instruction passed to FPU"

--ldu :: Maybe (RSEntry rob)
--    -> ¿MemRead?