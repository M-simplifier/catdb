# Verification scope

Native release baseline: Windows, GHC 9.6.7; Studio was built with Cabal 3.18.2.0.
CI also builds the native package on Linux.
This records bounded evidence, not a formal proof or production certification.

## Studio 0.3

The native Servant/Warp executable and real Miso 1.12.0.0 frontend compile.
The pinned Nix shell resolves GHC-WASM 9.14.1.20260330 (GHC source commit
05e0ef08e100cf3bd2150adcf8e53ba9ad30519a). The native CatDB library APIs and
store frame protocol are unchanged.

`test/studio_http.py EXECUTABLE COMPILED-WEB [LEGACY-FILE]` launches the real
server and checks local-session/origin protection, compiled WASM serving,
invalid reading-note draft rejection without a version/native-backup/file-size
change, valid atomic repair, immutable old versions, stale/recomputed views,
mapping validation, distinct Delta cards, Sigma classes, Pi compatible families,
empty Pi families, GeneratedRef witnesses, exact original-revision references,
definition export, native backup, close/reopen, explicit legacy adoption and
a custom Team→Office schema with deletion constraints. A real bounded Pi search
was cancelled through a concurrent HTTP request; another exceeded its operation
budget. Both saved no partial result. Caption tests follow actual PullRef and
GeneratedRef provenance; unrelated custom IDs receive no fixture caption.

The existing laws/Relay/native-memory/storage suite passed after integration.
Live browser acceptance of the running HTTP app covered split/merged endpoints,
disabled invalid apply, atomic apply and the Delta/source-revision journey.
The public site contains an explicitly labelled diagram and a separately labelled
actual Relay Desk screenshot; no Studio screenshot is fabricated.

The law suite enumerates component functions independently of the migration
algorithm, keeps the natural transformations, and checks both directions of
the two adjunction bijections. A fixed span and 30 seeded small cardinality
cases include empty fibers and nonidentity target arrows. It also checks
composition/identities, equation closure, invalid mappings, empty diagrams,
and natural actions. Indexed Pi handles a 160,000-choice diagram with 400
actual compatible families, beyond the former Cartesian-product cutoff.

Storage checks cover failed edits writing nothing, eight concurrent writers,
same/other-process exclusion, close/reopen equality, compact backups, partial
frame recovery and complete-frame corruption rejection. `test/crash.ps1`
provides a Windows process-kill check; this is not a power-loss test.

Relay model tests cover email identity, reversible explicit links, cross-team
assignment rejection, and removal of private/unused values from the entire
structured public export. The HTTP smoke test exercises persistence through
the actual server, rejection, handoff and native backup. Browser verification
includes CSV import with Japanese names, linking source identities, changed
candidate lists, assignment, and the public handoff. The website uses an actual
application screenshot with fictional example data.

No broad benchmark, SQLite performance claim, long-term endurance test, full
failure-injection matrix, or machine-checked proof is implied. The constraints
and remaining durability limits are in [STATUS](../STATUS.md) and [API](api.md).
