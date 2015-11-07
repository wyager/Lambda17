{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CPU.Defs (W(..), RIx(..), PC(..), Addr(..), Predicted(..), RVal(..), StationID(..)) where

import CLaSH.Prelude

newtype W = W {w :: BitVector 16} deriving (Num, Ord, Eq)

newtype PC = PC {pc :: BitVector 16} deriving (Num, Ord, Eq)

newtype RIx = RIx {rIx :: Index 16} deriving (Num, Enum, Ord, Eq)

newtype Addr = Addr {addrOf :: BitVector 16} deriving (Ord, Eq)

newtype Predicted a = Predicted a deriving (Show)

-- The register representation inside instructions.
-- rix is RIx (pre-dispatch) or StationID (post-dispatch)
-- Also used inside the register file, so we can copy it directly from
-- there to ops before putting them in registration stations
data RVal rix = Pending rix | Literal W deriving (Eq)

-- Used instead of RIx post-dispatch
data StationID fus stations = StationID (Index fus) (Index stations) deriving (Eq)

data Read = NoRead | Read Addr
data Fetch = NoFetch | Fetch Addr