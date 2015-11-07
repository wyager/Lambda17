module CPU.Defs (W(..), RIx(..)) where

import CLaSH.Prelude

data W = W {w :: BitVector 16} deriving (Num, Ord, Eq)

data PC = PC {pc :: BitVector 16} deriving (Num, Ord, Eq)

data RIx = RIx {rIx :: Index 16} deriving (Num, Ord, Eq)

data Addr = Addr {addrOf :: BitVector 16}

data Predicted a = Predicted a

-- The register representation inside instructions.
-- rix is RIx (pre-dispatch) or StationID (post-dispatch)
-- Also used inside the register file, so we can copy it directly from
-- there to ops before putting them in registration stations
data RVal rix = Pending rix | Literal W

-- Used instead of RIx post-dispatch
data StationID fus stations = StationID (Index fus) (Index stations)





-- Indended use: Like `Either x x` under `>>`
-- If there's a terminal value, it "wipes out" all
-- the nonterminals to the right of it
data Terminal x = Terminal x | Nonterminal x

instance Semigroup (Terminal x) where
    (Nonterminal _) <> x = x
    (Terminal x)    <> _ = Terminal x


data Read = NoRead | Read Addr
data Fetch = NoFetch | Fetch Addr