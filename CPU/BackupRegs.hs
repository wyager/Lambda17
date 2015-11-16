module CPU.BackupRegs () where

import CLaSH.Prelude hiding (empty)
import CPU.RegisterFile (RegisterFile(..))

data BackupRegs = BackupRegs (Vec 16 W)

empty :: BackupRegs
empty = BackupRegs (repeat 0x1337)

set :: RIx -> W -> BackupRegs -> BackupRegs
set ix w (BackupRegs regs) = BackupRegs $ replace ix w regs

restore :: BackupRegs -> RegisterFile rob
restore (BackupRegs regs) = RegisterFile $ map Literal regs

