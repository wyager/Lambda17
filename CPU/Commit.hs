module CPU.Commit () where

data Action = Write W RIx | Mispredict PC | MispredictNWrite W PC RIx


commit1 :: ROB (r+1) -> Action