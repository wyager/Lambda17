# Lambda17

A Cλash/Haskell implementation of an out-of-order superscalar CPU using
Tomasulo's algorithm. Follow-up to Lambda16 (a simpler in-order pipelined
design) for UT's CS 350C Advanced Architecture class.

## Architecture

The CPU is a Tomasulo machine with:

- **Reorder buffer** (ROB) for in-order commit — `CPU/ReorderBuffer.hs`
- **Reservation stations** per functional unit — `CPU/RStations.hs`
- **Register renaming** via the register file mapping to ROB IDs —
  `CPU/RegisterFile.hs`
- **Common data bus** (CDB) broadcast for result forwarding —
  `CPU/CDBBroadcast.hs`, `CPU/CDBMessage.hs`
- **Multiple functional unit types**: load units (LDU), FP units (FPU),
  control units (CU) — `CPU/FunctionalUnits.hs`
- **Backup register file** for misprediction recovery —
  `CPU/BackupRegs.hs`
- **Configurable superscalar dispatch** (N instructions per cycle) —
  `CPU/Dispatch.hs`

All sizes are type-level parameters. `Hardware.hs` pins them to concrete
values: 3 LDUs × 2 slots, 2 FPUs × 3 slots, 1 CU × 6 slots, 16-entry
instruction buffer, 16-entry ROB, 2-wide dispatch.

## Instruction set

Same 16-bit encoding as Lambda16 (see `CPU/InstructionSet.hs`):

| opcode | mnemonic      | encoding         |
|--------|---------------|------------------|
| 0      | Mov imm, r    | `0iii_iiii_iiii_rrrr` (8-bit immediate) |
| 1      | Add a, b, t   | `1aaa_abbb_btttt` |
| 2      | Jmp pc        | `2ppp_pppp_pppp` (12-bit PC) |
| 3      | Halt          | `3xxx_xxxx_xxxx` |
| 4      | Ld [addr], r  | `4aaa_aaaa_aarrrr` |
| 5      | Ldr a, b, t   | `5aaa_abbb_btttt` (t ← mem[r[a]+r[b]]) |
| 6      | Jeq a, b, pc  | `6aaa_abbb_bpppp` (4-bit relative PC) |

## Build

See `BUILD.md` for setup. Short version:

```bash
clash --verilog Hardware.hs -outputdir build
```

To simulate:

```bash
clash Main.hs -outputdir build && ./Main
```

Loads `mem.hex`, dumps the decoded program, and runs 10 cycles.
