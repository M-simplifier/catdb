# PoC status

CatDB 0.3 is a working proof of concept, not a production-stable database or a
drop-in SQLite replacement. CatDB Studio is a real local GUI over the database,
with a Miso/WASM Haskell frontend and native Servant/Warp backend. Relay Desk
remains a separate reference application using the same library and file format.

Supported: finite acyclic schema presentations, path equations, finite-set
instances, Delta/Sigma/Pi migration with witnesses and universal factor maps,
immutable native catalogs of schemas and data systems, checked system correspondences,
atomic validated edits, one-file persistence, recovery of incomplete tails,
and compact backups. One process owns a database at a time; threads share one
connection. Data and queries live in memory.

Studio supports generic schema construction, target selection, batched drafts,
atomic apply, search, deletion without cascade, checked reusable mappings,
real Delta/Sigma/Pi views, immutable revision history, original-revision row
witnesses, stale markers and explicit recomputation. It also exposes generated
subinstances, native backups, JSON definition/data export, and explicit adoption
of legacy files into a new file. The UI is Japanese. Fixture captions are display
labels; general scalar/string columns are not added. The record table displays
the first 100 search matches; equation lists display the first 200 matches and
count all applicable rows. Validation covers the entire draft.

Studio uses core defaultBudget and a 20-second timeout for pure mutation/preview
workers. Cancellation before disk commit saves nothing; an ongoing disk commit
cannot be cancelled. Revision checks include the open-file generation. Imported
multi-step views are preserved and inspectable; single-step recompute refuses
to silently replace their chains. Native catalogs combined with other application
data preserve that outer data. Editing a catalog snapshot does not synchronize
it back into the other application's outer state.

Studio binds only to loopback. Each launch uses an OS-random session token;
API reads and writes require it, with Host/origin checks and bounded input.
This is a local single-user application, without remote access or account login.

Limits: no schema cycles, SQL, query optimizer, indexes on disk, network or
multi-process service, encryption, or automatic schema upgrades. The category
compiler defaults to at most 4,096 raw paths. Explicit configurable budgets also
bound schema size and equation work. Sigma defaults to 100,000 transported tokens
per migration. Pi defaults to at most 1,000,000 search nodes and 100,000 result
families across all destination objects; exceeding a bound returns a typed error
instead of a partial result. Catalog decoding also bounds records and replay.
See [native catalogs](docs/native-memory.md). Large files and migrations can exhaust
memory. The on-disk format is versioned by its frame marker, but compatibility
across future PoC versions is not promised. Keep backups before upgrading.

Successful writes flush the file before acknowledgement. Complete corrupt
frames fail closed; an incomplete final frame is discarded on open. This is
not a claim of SQLite's maturity or power-loss certification. The checksum
detects accidental corruption, not hostile tampering. Use local disks with
working OS locking and flush semantics.

Relay Desk is a single-user loopback application. It does not implement login,
authorization, or remote access. Shared identity is an explicit policy: equal
normalized email addresses coalesce, and users may add reversible links. Pi
enumerates eligible people; it does not optimize or decide assignments. Public
exports contain request titles, teams, states, and assigned display names;
review these contents before sharing.
