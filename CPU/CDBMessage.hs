{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module CPU.CDBMessage (CDBMessage(..), CDBData(..)) where

import Clash.Prelude
import CPU.Defs (RobID, RIx, W, PC)

data CDBMessage r = CDBMessage (RobID r) CDBData deriving (Show, Generic, NFDataX)

data CDBData = RegWrite W RIx
             | JumpTaken PC
             | JumpNotTaken
             | DoHalt deriving (Show, Generic, NFDataX)