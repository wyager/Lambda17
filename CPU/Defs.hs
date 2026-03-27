{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ConstraintKinds #-}

module CPU.Defs (
    S, Clk,
    W(..), RIx(..), PC(..), Addr(..),
    Predicted(..), RVal(..), StationID(..),
    RobID(..), Read(..), MemRead(..), Jump(..),
    Halt(..), Fetch(..)) where

import Clash.Prelude hiding (Read)
import Text.Printf (printf)

-- Modern Clash needs an explicit clock domain on Signal. S is a shorthand
-- for Signal System; Clk aliases the constraint needed by sequential
-- primitives like register.
type S a = Signal System a
type Clk = HiddenClockResetEnable System

newtype W = W {w :: BitVector 16}
    deriving newtype  (Num, Ord, Eq)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)

newtype PC = PC {pc :: BitVector 16}
    deriving newtype  (Num, Ord, Eq, Enum)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)

newtype RIx = RIx {rIx :: Index 16}
    deriving newtype  (Num, Enum, Ord, Eq)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)

newtype Addr = Addr {addrOf :: BitVector 16}
    deriving newtype  (Ord, Eq, Enum)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)

newtype Predicted a = Predicted a
    deriving newtype  (Eq)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)

data Jump = NoJump | Jump PC deriving (Show, Eq, Generic, NFDataX)

data Halt = DontHalt | DoHalt deriving (Show, Eq, Generic, NFDataX)

-- The register representation inside instructions.
-- rix is RIx (pre-dispatch) or StationID (post-dispatch)
-- Also used inside the register file, so we can copy it directly from
-- there to ops before putting them in registration stations
data RVal rix = Pending rix | Literal W deriving (Eq, Show, Generic, NFDataX)

-- Used instead of RIx post-dispatch
data StationID fus stations = StationID (Index fus) (Index stations)
    deriving (Eq, Show, Generic, NFDataX)

newtype RobID n = RobID (Index n)
    deriving newtype  (Show, Eq, Ord)
    deriving stock    (Generic)
    deriving anyclass (NFDataX)
instance KnownNat n => Enum (RobID n) where
    toEnum = RobID . toEnum
    fromEnum (RobID n) = fromEnum n
    succ (RobID n) | n == maxBound = RobID 0
                   | otherwise     = RobID (n + 1)
    pred (RobID 0) = RobID maxBound
    pred (RobID n) = RobID (n-1)

data Read = NoRead | Read Addr deriving (Show, Generic, NFDataX)
data Fetch = NoFetch | Fetch PC deriving (Show, Generic, NFDataX)

data MemRead = NothingRead | ReadSome W deriving (Show, Eq, Generic, NFDataX)

instance Show Addr where show (Addr a) = printf "[Addr %x]" (fromEnum a)
instance Show PC where show (PC a)     = printf "[PC %x]"   (fromEnum a)
instance Show W where show (W a)       = printf "[W %x]"    (fromEnum a)
instance Show RIx where show (RIx a)   = printf "[RIx %x]"  (fromEnum a)
instance Show a => Show (Predicted a) where 
    show (Predicted a) = printf "[Pred %s]" (show a)