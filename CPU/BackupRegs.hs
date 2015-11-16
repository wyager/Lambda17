module CPU.BackupRegs (BackupRegs, empty, set, restore) where

import CLaSH.Prelude hiding (empty)
import CPU.RegisterFile (RegisterFile(RegFile))
import CPU.Defs (W, RIx, RVal(Literal))

data BackupRegs = BackupRegs (Vec 16 W) deriving (Show, Eq)

empty :: BackupRegs
empty = BackupRegs (repeat 0x1337)

set :: RIx -> W -> BackupRegs -> BackupRegs
set ix w (BackupRegs regs) = BackupRegs $ replace ix w regs

restore :: BackupRegs -> RegisterFile rob
restore (BackupRegs regs) = RegFile $ map Literal regs

