module CPU.RStations (RStations, freeSlot, insert) where

import CLaSH.Prelude
import CPU.Defs (StationID(..))
import CPU.Op (Op)

type KN f s = (KnownNat f, KnownNat s)

-- That's an earful!
data RStations fus stations = RStations (Vec fus (Vec stations (Maybe (Op (StationID fus stations)))))

freeSlot :: KN f s => RStations f s -> Index f -> Maybe (Index s)
freeSlot (RStations fus) fu = elemIndex Nothing (fus !! fu)

insert :: KN f s => StationID f s -> Op (StationID f s) -> RStations f s -> RStations f s
insert (StationID fu station) op (RStations vec) = RStations $ replace fu stations' vec
    where stations' = replace station (Just op) (vec !! fu)

delete :: KN f s => StationID f s -> RStations f s -> RStations f s
delete (StationID fu station) (RStations vec) = RStations $ replace fu stations' vec
    where stations' = replace station Nothing (vec !! fu)