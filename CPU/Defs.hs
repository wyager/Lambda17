{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CPU.Defs (
    W(..), RIx(..), PC(..), Addr(..), 
    Predicted(..), RVal(..), StationID(..), 
    RobID(..), Read(..), MemRead(..)) where

import CLaSH.Prelude hiding (Read)
import Text.Printf (printf)

newtype W = W {w :: BitVector 16} deriving (Num, Ord, Eq)

newtype PC = PC {pc :: BitVector 16} deriving (Num, Ord, Eq)

newtype RIx = RIx {rIx :: Index 16} deriving (Num, Enum, Ord, Eq)

newtype Addr = Addr {addrOf :: BitVector 16} deriving (Ord, Eq)

newtype Predicted a = Predicted a deriving (Eq)

-- The register representation inside instructions.
-- rix is RIx (pre-dispatch) or StationID (post-dispatch)
-- Also used inside the register file, so we can copy it directly from
-- there to ops before putting them in registration stations
data RVal rix = Pending rix | Literal W deriving (Eq, Show)

-- Used instead of RIx post-dispatch
data StationID fus stations = StationID (Index fus) (Index stations) deriving (Eq, Show)

newtype RobID n = RobID (Index n) deriving (Show, Eq, Ord)
instance KnownNat n => Enum (RobID n) where
    toEnum = RobID . toEnum 
    fromEnum (RobID n) = fromEnum n
    succ (RobID n) | n == maxBound = RobID 0
                   | otherwise     = RobID (n + 1)
    pred (RobID 0) = RobID maxBound
    pred (RobID n) = RobID (n-1)

data Read = NoRead | Read Addr deriving Show
data Fetch = NoFetch | Fetch Addr deriving Show

data MemRead = NothingRead | ReadSome W deriving Show

instance Show Addr where show (Addr a) = printf "[Addr %x]" (fromEnum a)
instance Show PC where show (PC a)     = printf "[PC %x]"   (fromEnum a)
instance Show W where show (W a)       = printf "[W %x]"    (fromEnum a)
instance Show RIx where show (RIx a)   = printf "[RIx %x]"  (fromEnum a)
instance Show a => Show (Predicted a) where 
    show (Predicted a) = printf "[Pred %s]" (show a)