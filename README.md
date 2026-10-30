# CatDB

**Open your database. Follow the connections.**

CatDB is an embedded Haskell database of finite functors. Schemas describe
objects, total arrows and equal paths. Checked schema mappings induce real
Δ views, Σ quotients and Π compatible families. Your data, immutable revisions,
mappings and provenance live in one native `.catdb` file.

**Working proof of concept · MIT · contributions through issues only**

[Website](https://m-simplifier.github.io/catdb/) ·
[公式サイト（日本語）](https://m-simplifier.github.io/catdb/ja/) ·
[Download v0.3.0-poc](https://github.com/M-simplifier/catdb/releases/tag/v0.3.0-poc) ·
[Studio guide](docs/studio.md) · [Studioの使い方](docs/studio.ja.md) ·
[Limits](STATUS.md) · [Mathematics](docs/mathematics.md) · [Native catalogs](docs/native-memory.md)

## Start with CatDB Studio

Extract the Windows package and double-click **Start CatDB Studio.cmd**.
No compiler or separate database service is needed. The native application
opens its Miso/WASM interface in your browser. Keep `studio-web` beside the
executable. The interface is Japanese; the English guide covers the full entry.

Create a reading-notes fixture, a merge/compatible-family fixture, or a blank
database and your own finite acyclic schema. Open an existing `.catdb` by path.
A bare filename is saved under `data` beside the executable; the active absolute
path is shown in Files/export. Close and reopen reliably, or export a native
backup. Legacy files without a catalog open read-only and can be adopted into
a new file while preserving the original.

Try the reading fixture:

1. Select `Note / n1`: `book` and `quote → quoteBook` both reach `Book / b1`.
2. Change only `book` to `b2`. The focused diagram splits. The real engine
   rejects that draft; the committed revision stays unchanged.
3. Change `quote` to `q2` too, check the whole draft, then apply both changes
   atomically as a new revision. No relationship is silently repaired.
4. Create a Δ view using `cards`. Three notes remain three cards, even when
   two refer to the same book. Follow a card's provenance into its source
   revision. Subsequent source edits mark the old view stale; recompute explicitly.

The generic editors construct objects/arrows/equations, add/edit/delete records,
build checked mappings, and execute Δ, Σ and Π. Result rows expose PullRef,
SigmaClass, PiFamily (including empty families) and GeneratedRef witnesses.
Σ does not establish real-world identity; Π does not establish factual truth.
Fixture captions are fictional display labels; this release adds no generic
scalar/string column system. [Detailed walkthrough](docs/studio.md).

```powershell
.\catdb-studio.exe --browser
# Optional: --file C:\data\notes.catdb --data-dir C:\data --port 4284
```

The launch URL contains a fresh local session token. The server binds only to
loopback. Ctrl+C or closing its console stops it. Drafts are unsaved browser
proposals; the native Haskell engine validates, checks the expected revision,
and commits. Native `.catdb` remains canonical, without an authoritative JSON
or SQLite sidecar. [Definition export example](examples/StudioExport.hs).

## Relay Desk is an application example

Relay Desk uses the same library to reconcile CSV rosters, enumerate team-compatible
assignees and export a public handoff. It remains included; its application
policies are distinct from CatDB's generic operations.

![Actual Relay Desk reference application](site/assets/relay-desk.jpg)

Run `Start Relay Desk.cmd`, or `relay-desk.exe desk.catdb --demo`.
See the [Relay Desk guide](docs/guide.ja.md), [reference model](reference/src/Relay/Model.hs)
and `samples/` CSV files. Equal normalized emails and explicit links are this
application's identification policy; Π enumerates compatible candidates, without
optimizing or choosing assignments. Sharing excludes fields through explicit
view and reachability definitions; Δ alone is not a privacy filter.

## What the category theory buys

Conventional databases can implement this workflow too. CatDB explores whether
**one model of schema translation** makes these operations easier to specify,
inspect and compose.

| Operation | General meaning | Relay Desk use |
|---|---|---|
| `delta f` (Δ) | Precompose with a schema map | Read paths into a public schema |
| `sigma f` (Σ) | Left Kan extension: quotient transported elements | Resolve identities, retaining origins |
| `piMigration f` (Π) | Right Kan extension: compatible families | Enumerate memberships and requests agreeing on team |
| `generated roots` | Least subfunctor containing roots | Trim values unreachable from the public handoff |

For a checked schema map `F : C → D`, the migrations satisfy **Σ_F ⊣ Δ_F ⊣ Π_F**.
The API includes natural transformations, units/counits, `sigmaFactor`,
`piFactor`, and witnesses: the universal properties are callable, not just names.
The app uses different schema maps for different tasks. Its person/team
membership table is application assembly; Π constructs the compatible families.
It does not optimize assignments. Δ alone is not a privacy filter; explicit
root selection and reachability are essential.

## Embed it

Use `import CatDB` and add this directory as a local Cabal package. `Schema`,
`Instance`, `Mapping`, and `Natural` are abstract checked types. Schemas are
runtime values, not type-level Haskell indices.

```haskell
-- Pure transformations; mappings and roots are application values.
public <- delta publicMapping workspace
handoff <- generated visibleRoots public
resolved <- sigma identityMap roster
candidates <- piMigration eligibilityMap memberships

-- A pure callback inside a durable transaction.
result <- transaction db $ \current -> do
  next <- edit changes current
  pure ((), next)
```

See the [complete runnable example](examples/Embedded.hs),
[reference model](reference/src/Relay/Model.hs), and [API contract](docs/api.md).

```sh
cabal run embedded-example -- example.catdb
cabal run catdb -- demo tasks.catdb
cabal run catdb -- todo tasks.catdb add milk home alice "Buy milk"
cabal run catdb -- todo tasks.catdb list
cabal run catdb -- query tasks.catdb delta examples/public-view.json cards.catdb
```

## Build

Native release baseline: GHC 9.6.7. Windows: `./build-frontend.ps1`, then `./build.ps1 -Test` (use `-Compiler` when GHC 9.6.7 is not on PATH).
The result includes `build/catdb-studio.exe`, `build/studio-web/`, the CLI and Relay Desk. The frontend uses Miso 1.12.0.0 and the pinned Nix/GHC-WASM shell; the native build never imports Miso.
With Cabal on Linux or Windows:

```sh
cabal update
cabal build all
cabal test laws --test-show-details=direct --test-options=build/test-data
cabal run relay-desk -- desk.catdb --demo --assets reference/web
```

Use a fresh directory for each manual test run. Warp is pinned to 3.4.12:
3.4.13's receive wait stalled on the tested GHC/Windows runtime. The database
library has no Warp dependency. See [verification](docs/verification.md).

## Deliberate limits

Finite **acyclic** schemas, in-memory data, one exclusive connection, bounded
migrations. No SQL, disk indexes, remote service, encryption, or automatic
format upgrades. This is not a drop-in SQLite replacement or a production
maturity claim. [STATUS.md](STATUS.md) describes the exact boundary.

Original code, UI and website: [MIT](LICENSE). Binary dependencies retain their
own licenses, including GMP; releases include notices and corresponding source.
See [licensing](docs/licensing.md).

Open an [issue](https://github.com/M-simplifier/catdb/issues) with a bug, real
use case or mathematical counterexample. **External PRs are not accepted.**
[Contribution policy](CONTRIBUTING.md).

Theory: David Spivak's [Functorial Data Migration](https://arxiv.org/abs/1009.1166).
[CQL](https://categoricaldata.net/CQL/) is prior work. This is an independent,
deliberately smaller implementation and includes no CQL source code.
