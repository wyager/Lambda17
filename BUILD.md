# Building Lambda17 with Modern Clash

Lambda17 was originally written for CλaSH 0.x. It has been ported to
**Clash 1.8.2** with GHC 9.4.

## Requirements

Same as Lambda16 — GHC 9.0–9.6, `clash-ghc-1.8.2`, `clash-prelude-1.8.2`
(pinned), plus the three typechecker plugins. Or use the `fmax-hdl`
Docker image.

## Generate HDL

```bash
clash --verilog Hardware.hs -outputdir build
```

**Note**: The original author's last commit message was "Simulation
working, but not hardware..." — the HDL generation may hit issues
even after the Clash-version port. The large `CPUState` type (10 type
parameters, deeply nested) is a stress test for Clash's synthesis.

## Simulate

```bash
clash Main.hs -outputdir build && ./Main
```

## What changed from CλaSH 0.x

- **Module rename**: `CLaSH.Prelude` → `Clash.Prelude`.
- **Clock domains**: `Signal a` → `Signal System a` (aliased as `S a`
  in `CPU/Defs.hs`). `cpu'`, `mem`, `fetcher`, `withRAM` gained
  `HiddenClockResetEnable System` constraints (aliased as `Clk`).
- **NFDataX**: The entire `CPUState` tree — `OpBuffer`, `RegisterFile`,
  `RStations`, `ROB`, `FUStates`, `BackupRegs`, `FetchState`, plus
  every type they contain (`Buffer`, `Count`, `Waiting`, `RSEntry`,
  `LDUState`, `Op`, `Fetched`, `CDBMessage`, `CDBData`, all `Defs.hs`
  types) — now derives `Generic` + `NFDataX`. This was the bulk of
  the port: 15+ data declarations touched.
- **Buffer.empty**: The ring buffer's default value used `error`, which
  modern Clash's `register` rejects. Changed to `errorX` with an
  `NFDataX a` constraint.
- **topEntity**: Rewritten with explicit `Clock/Reset` ports and a
  `Synthesize` annotation. The body is factored into an `adapter`
  function so `withClockResetEnable` scopes correctly over `cpu'`.
- **Simulation**: `Main.hs` wraps `withRAM` with the simulation clock
  generators.
