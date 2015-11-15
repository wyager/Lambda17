module CPU.CDBMessage (CDBMessage(..), CDBData(..)) where

import CLaSH.Prelude
import CPU.Defs (RobID, RIx, W, PC)

data CDBMessage r = CDBMessage (RobID r) CDBData deriving (Show)

data CDBData = RegWrite W RIx
             | JumpTaken PC
             | JumpNotTaken
             | DoHalt deriving (Show)