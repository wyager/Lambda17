# Lambda17 — Tomasulo Out-of-Order CPU

A 16-bit CPU implementing Tomasulo's algorithm with register renaming, a
reorder buffer, and speculative execution. Written in Clash and compiled
to Verilog.

## Pipeline

```
 ┌──────┐  ┌──────────┐  ┌──────────┐  ┌────────┐  ┌─────────┐  ┌────────┐
 │Fetch ├─▶│ OpBuffer ├─▶│ Dispatch ├─▶│RStation├─▶│Func Unit├─▶│  CDB   │
 └──────┘  └──────────┘  └────┬─────┘  └────────┘  └─────────┘  └───┬────┘
                              │                                     │
                              ▼                                     │
                         ┌─────────┐                                │
                         │   ROB   │◀───────────────────────────────┘
                         └────┬────┘           broadcast
                              │
                              ▼
                         ┌─────────┐
                         │ Commit  │──▶ BackupRegs (architectural state)
                         └─────────┘
```

Instructions flow in program order into the ROB, execute out of order in
functional units, and retire in program order from the ROB.

## Per-cycle data flow (`Playground.cpu`)

| # | Stage     | Module           | What happens |
|---|-----------|------------------|--------------|
| 1 | Fetch     | `CPU.Fetch`      | Issue memory reads for the next `d` instructions. |
| 2 | Buffer    | `CPU.OpBuffer`   | Decoded instructions go into a FIFO keyed by fetch-PC. |
| 3 | Dispatch  | `CPU.Dispatch`   | Up to `d` instructions: look up source operands in the register file, allocate ROB slot, insert into a reservation station, rename the destination register to its ROB slot. `Ldr` decomposes into a virtual `Add` + dependent `Ld`. |
| 4 | Execute   | `CPU.FunctionalUnits` | Each FU scans its station column for a *grounded* entry (all operands `Literal`), executes it, removes it from the station, and produces a `CDBMessage`. |
| 5 | Broadcast | `CPU.CDBBroadcast` | Every CDB message updates (a) the register file, (b) all reservation-station entries that were `Pending` on that ROB slot, and (c) the ROB entry itself, marking it `Done` with its committed form. |
| 6 | Commit    | `CPU.Commit`     | Pop up to `d` `Done` entries from the ROB head in program order. `Mov` writes backup regs; `Jmp` checks prediction; `Halt` stops; `Nop` retires silently. A mispredict flushes the pipeline and restores from backup. |

## Ldr decomposition

`Ldr rA rB rT` (load from address `rA + rB` into `rT`) is not a hardware
primitive. Dispatch decomposes it into two micro-ops:

1. A **virtual Add** computes `rA + rB`. It goes into a reservation
   station so the FPU executes it and broadcasts the sum on the CDB.
2. A **dependent Ld** waits for the Add's ROB slot, picks up the address
   from the CDB, and loads from memory into `rT`.

The virtual Add has no architectural destination. Its ROB slot holds `Nop`
instead of `Add`, so `commit1` retires it without touching backup regs.
The Add itself still lives in the reservation station, executes, and
broadcasts — so the Ld still receives the computed address.

## Functional units

Three FU types share a 3-column reservation-station matrix:

| `select` | FU type | Ops          | Latency | CDB data                    |
|----------|---------|--------------|---------|-----------------------------|
| 0        | FPU     | `Mov`, `Add` | 1 cycle | `RegWrite w rix`            |
| 1        | LDU     | `Ld`         | ≥2 cycles | `RegWrite w rix`          |
| 2        | CU      | `Jmp`,`Jeq`,`Halt` | 1 cycle | `JumpTaken`/`JumpNotTaken`/`DoHalt` |

The LDU is stateful (`LDUState = Loading rix rob | Empty`) and holds a
load across the memory round-trip.

## Invariants

- **In-order commit**: `robPop` only returns the head entry, and only when
  it is `Done`. This guarantees precise exceptions and correct
  mispredict recovery.
- **Register file / backup split**: the register file holds *speculative*
  renames (`Pending robID` | `Literal w`). Backup regs hold *committed*
  architectural state. On mispredict, `reset` copies backup → register
  file.
- **ROB slot uniqueness**: `robInsert` allocates slots in order and
  `RobID` wraps cyclically. Because commit is in-order, a slot is never
  reused while anything still depends on it.
- **Disjoint Add/Ld stations**: `Dispatch` assumes `select (Add …) ≠
  select (Ld …)` so the two halves of an Ldr decomposition land in
  different station columns and can't collide.
- **Nop retires silently**: `Nop` appears only in the ROB, only as the
  placeholder for a virtual Add. It never reaches a functional unit or a
  reservation station.

## Configuration (`Hardware.hs`)

| Parameter | Type-level name | Value |
|-----------|-----------------|-------|
| Load units (LDU) | `LDUs` | 3 |
| Slots per LDU | `LDUSlots` | 2 |
| FP units (FPU) | `FPUs` | 2 |
| Slots per FPU | `FPUSlots` | 3 |
| Control units (CU) | `CUs` | 1 |
| Slots per CU | `CUSlots` | 6 |
| Station column height | `Height` | 6 |
| Op-buffer depth | `IBuf` | 16 |
| Reorder-buffer depth | `RBuf` | 16 |
| Dispatches/commits per cycle | `DispatchPerCycle` | 2 |

The `FUsC` constraint requires `LDUs × LDUSlots = FPUs × FPUSlots = CUs ×
CUSlots = Height` so that each FU type's station slots tile evenly into
its column.

## Instruction encoding

16-bit instruction word, big-endian fields:

```
 15      12 11       8 7        4 3        0
 ┌────────┬──────────┬──────────┬──────────┐
 │ opcode │    rA    │    rB    │    rT    │
 └────────┴──────────┴──────────┴──────────┘
```

| Opcode | Mnemonic | Fields            | Meaning |
|--------|----------|-------------------|---------|
| 0      | Mov      | imm[11:4] rT      | `rT ← imm` (zero-extended) |
| 1      | Add      | rA rB rT          | `rT ← rA + rB` |
| 2      | Jmp      | pc[11:0]          | Unconditional jump |
| 3      | Halt     | —                 | Stop |
| 4      | Ld       | addr[11:4] rT     | `rT ← mem[addr]` |
| 5      | Ldr      | rA rB rT          | `rT ← mem[rA + rB]` |
| 6      | Jeq      | rA rB pc[3:0]     | Jump if `rA == rB` |
