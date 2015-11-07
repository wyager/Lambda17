module CPU.ReorderBuffer () where

data ROB n f s = ROB (Buffer n (Waiting f s))

data Waiting f s = Waiting (Fetched (Op (StationID f s))) (StationID f s) Bool

oneFree :: ROB n f s -> Bool
oneFree (ROB buf) = not (full buf)

twoFree :: ROB n f s -> Bool
twoFree (ROB buf) = fromEnum (maxCount buf) - fromEnum (count buf) >= 2

waitFor :: Fetched (Op (StationID f s)) -> StationID f s -> ROB n f s -> ROB n f s
waitFor op from (ROB buf) = Buf.insert' buf $ Waiting op stationID False