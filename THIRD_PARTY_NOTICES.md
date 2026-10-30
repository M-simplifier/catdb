# Third-party components

CatDB's original library, Studio, Relay Desk, examples and website are MIT.
No CQL code is incorporated. System fonts are used; no web fonts, CDN scripts,
third-party images or sign-in SDKs are bundled in Studio.

The frontend uses Miso 1.12.0.0 (BSD-3-Clause), resolved commit
`9f1222293fe92e7d22272e884ac97c205876b943`, and
`@bjorn3/browser_wasi_shim` 0.3.0 (MIT/Apache-2.0). Their original license files
are in `studio/frontend/static/licenses/`. The generated FFI loader and WASM
also contain code from the pinned GHC-WASM runtime and WASI/LLVM libraries.
`WASM_NOTICES.txt` and `WASM_DEPENDENCIES.json` record their versions, source
locators and notice texts. The WASM compiler's selected bignum backend is Native.

The Windows native executables use GHC 9.6.7 and its GMP 6.3.0 backend.
GMP is conveyed under LGPL-3.0-or-later. The binary release retains full native
dependency notices, GNU license texts, matching dependency-source archives and
the official GHC source archive containing GMP source and patches. Rebuilding
and modification/recombination instructions accompany the corresponding-source
archive. Modification and reverse engineering for debugging modifications are
permitted. The MIT label does not relicense dependencies.

Regenerate frontend notices with `scripts/collect_wasm_notices.py` against the
actual frontend plan before packaging. `package-studio.ps1` collects native
notices from the actual build plan and refuses missing dependency sources or
license texts. See [licensing](docs/licensing.md) for the distribution boundary.
