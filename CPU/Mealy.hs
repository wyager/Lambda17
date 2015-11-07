module CPU.Mealy () where

data State = State FUState Registers

data FUState = FUState FetchState LoadState FPUState 

data ROB n = Buffer n (FetchedOp)

data Output = Output Fetch Read

data Input = Input Fetched Readed

step :: State -> Input -> State
step (State f l m r) = State f' l' m' r'

data Stations fus stations = Vec fus (Vec stations Op)



