# Building Lambda17

## Requirements

Same as Lambda16 — GHC 9.0–9.6, `clash-ghc-1.8.2`, `clash-prelude-1.8.2`
(pinned), plus the three typechecker plugins. Or use the `fmax-hdl`
Docker image.

## Generate HDL

```bash
clash --verilog Hardware.hs -outputdir build
```

## Simulate

```bash
clash Main.hs -outputdir build && ./Main
```
