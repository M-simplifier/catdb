#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "$0")"
if ! command -v wasm32-wasi-cabal >/dev/null 2>&1; then
  exec nix develop .#wasm --command bash ./build.sh "$@"
fi
wasm32-wasi-cabal build exe:studio-gui
wasm_binary="$(wasm32-wasi-cabal list-bin exe:studio-gui | tail -n 1)"
wasm_library="$(wasm32-wasi-ghc --print-libdir)"
web_output="${CATDB_WEB_OUTPUT:-../web}"
mkdir -p -- "$web_output"
cp -R static/. "$web_output/"
"$wasm_library/post-link.mjs" --input "$wasm_binary" --output "$web_output/ghc_wasm_jsffi.js"
cp "$wasm_binary" "$web_output/studio-gui.wasm"
if command -v wasm-opt >/dev/null 2>&1; then
  wasm-opt -all -O2 "$web_output/studio-gui.wasm" -o "$web_output/studio-gui.wasm"
fi
if command -v wasm-tools >/dev/null 2>&1; then
  wasm-tools strip -o "$web_output/studio-gui.wasm" "$web_output/studio-gui.wasm"
fi
printf 'Built %s/studio-gui.wasm (%s bytes)\n' "$web_output" "$(wc -c < "$web_output/studio-gui.wasm")"
