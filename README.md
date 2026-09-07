# Fourier / NTT visualizer — OCaml

DFT (Complex) and NTT (int64 mod p) visualizer: draw on a 16x16 grid,
see the transformation matrix and the transformed output as heatmaps.

## Architecture

Deliberately follows the "single write channel" design discussed:

- `model` — immutable record (pixels are replaced, never mutated in place)
- `action` — `Draw | Clear | Set_mode`, the only way to change the model
- `apply`  — the single pure update function
- The only "global" is the Note `signal` holding the model; UI code can
  only *send* actions. Renders always read the current signal value.

## Build & run

    opam install ocaml dune brr note
    dune build

    # serve (wasm_of_ocaml target)
    dune build @default # produces bin/main.js
    python3 -m http.server 8080
    # open http://localhost:8080 — index.html loads bin/main.js

## Layout

    lib/dft.ml   — DFT matrix + 2D transform (Complex.t, stdlib)
    lib/ntt.ml   — NTT matrix + 2D transform (int64, mod p)
    bin/main.ml  — reactive UI (note/brr), canvas drawing

## Serving

    cp _build/default/bin/main.bc.js _build/default/bin/main.bc.js.runtime.js bin/
    python3 -m http.server 8080

index.html loads bin/main.bc.js (ES module). The runtime file
main.bc.js.runtime.js must sit next to it — jsoo loads it at startup.

## Quick serve

    ./serve.sh

(handles the dune build, copies _build/default/bin/main.bc.js into bin/,
and starts the HTTP server — run from the project root.)
