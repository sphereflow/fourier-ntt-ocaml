#!/bin/sh
# Build (if needed), copy the jsoo output where index.html expects it, serve.
rm -f bin/main.bc.js
set -e
dune build
mkdir -p bin
cp _build/default/bin/main.bc.js bin/
# some jsoo versions emit a separate runtime file — copy it too if present
cp _build/default/bin/main.bc.js.runtime.js bin/ 2>/dev/null || true
echo "serving on http://localhost:8080 — Ctrl-C to stop"
python3 -m http.server 8080
