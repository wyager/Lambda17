module RegisterFile () where

data RegisterFile fus stations = RegFile (Vec 16 (RVal (StationID fus stations)))

renameReg :: RIx -> StationID f s -> RegisterFile f s -> RegisterFile f s
renameReg (RegFile vec) (RIx rix) newName = RegFile $ replace rix (Pending newName) vec
