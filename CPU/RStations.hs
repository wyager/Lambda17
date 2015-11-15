module CPU.RStations (RStations, freeSlot, insert, empty, RSEntry(..)) where

import CLaSH.Prelude hiding (empty)
import CPU.Defs (StationID(..), RobID(..))
import CPU.Op (Op)

type KN f s r = (KnownNat f, KnownNat s, KnownNat r)

data RSEntry r = RSEntry (Op (RobID r)) (RobID r) deriving (Show, Eq)

-- That's an earful!
data RStations fus stations rob = RStations 
                                  (Vec fus 
                                  (Vec stations 
                                  (Maybe 
                                  (RSEntry rob)))) deriving (Show, Eq)

empty :: KN f s r => RStations f s r
empty = RStations $ repeat $ repeat $ Nothing

freeSlot :: KN f s r => RStations f s r -> Index f -> Maybe (Index s)
freeSlot (RStations fus) fu = elemIndex Nothing (fus !! fu)

insert :: KN f s r => StationID f s -> RSEntry r -> RStations f s r -> RStations f s r
insert (StationID fu station) op (RStations vec) = RStations $ replace fu stations' vec
    where stations' = replace station (Just op) (vec !! fu)

delete :: KN f s r => StationID f s -> RStations f s r -> RStations f s r
delete (StationID fu station) (RStations vec) = RStations $ replace fu stations' vec
    where stations' = replace station Nothing (vec !! fu)