# Verification scope

Release baseline: Windows, GHC 9.6.7, Cabal 3.12.1.0. CI also builds on Linux.
This records bounded evidence, not a formal proof or production certification.

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
