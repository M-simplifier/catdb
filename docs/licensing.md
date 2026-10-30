# Licensing and binary notices

CatDB's original Haskell/C source, Studio and Relay Desk UI, documentation, sample data and
website are under the [MIT license](../LICENSE). No CQL code is incorporated.
The mathematical reference is credited, not copied. The UI uses system fonts,
original styles and an actual screenshot with fictional data; there are no
third-party images, CDN scripts or bundled font files.

The Windows release is inspected against the resolved Cabal dependency
closure and the installed GHC toolchain. The dependency manifest records exact
Haskell/runtime names, versions and declared licenses. The distribution includes their
full available license files in THIRD_PARTY_NOTICES.txt, plus MinGW runtime,
LLVM and libffi notices. The dependency manifest records names and versions.
These licenses remain in force; the MIT label does not relicense dependencies.

Studio also distributes Miso 1.12.0.0 (BSD-3-Clause), the generated GHC-WASM
runtime/FFI loader, and browser_wasi_shim 0.3.0 (MIT/Apache-2.0). Original Miso
and shim notices, plus collected GHC/WASI/LLVM notice texts and a frontend
dependency manifest, are under studio-web/licenses in the package. The pinned
WASM compiler uses the Native bignum backend; the native executables use GMP.
The WASI SDK binary distribution is identified by its upstream build timestamp;
the notice manifest distinguishes license-text source revisions from a libc
binary revision that was not independently established.

**GMP needs additional treatment.** GHC 9.6.7 on this host uses the GMP backend
and statically includes GMP 6.3.0. It is distributed under LGPL-3.0-or-later
(the LGPL option of its dual license). The release includes GPLv3 and LGPLv3
texts and a corresponding-source bundle alongside the binary download.

The source bundle contains this application's source, the matching dependency
source archives, and the official GHC 9.6.7 source archive, including its GMP
6.3.0 source and build patches. REBUILD.md explains rebuilding with a modified
GMP. No restriction on modification or reverse engineering for debugging such
modifications is imposed. Keep these materials and notices with redistributed
binaries. The source project itself remains MIT.

Release materials distinguish the original MIT project from dependencies;
consult the included texts for their exact terms:
[MIT](https://opensource.org/license/mit),
[GHC](https://www.haskell.org/ghc/license.html),
[GMP licensing](https://gmplib.org/manual/Copying),
[LGPLv3](https://www.gnu.org/licenses/lgpl-3.0.html).
