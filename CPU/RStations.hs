module RStations () where

-- That's an earful!
data RStations fus stations = RStations (Vec fus (Vec stations (Maybe (Op (StationID fus stations)))))

freeSlot :: RStations f s -> Index f -> Maybe (Index s)
freeSlot (RStations fus) fu = elemIndex Nothing (fus !! fu)

insert :: StationID f s -> Op (StationID f s) -> RStations f s -> RStations f s
insert (StationID fu station) op (RStations vec) = RStations $ replace fu stations' vec
    where stations' = replace station (Just op) (vec !! fu)