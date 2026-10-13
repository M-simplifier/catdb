# API and storage contract

Import `CatDB`. [Embedded example](../examples/Embedded.hs) ·
[Application model](../reference/src/Relay/Model.hs).

| Area | Entry points |
|---|---|
| Schema | `schema`, `endpoint`, `compose`, `equivalent`, `mapping`, `composeMapping` |
| Instance | `instanceOf`, `empty`, `edit`, `elements`, `evaluate`, `rows` |
| Subinstances | `select` (greatest restriction), `generated` (least closure) |
| Migration | `delta`, `sigma`, `piMigration`, witnesses, universal factor maps |
| Queries | `pull`, `push`, `joinOver`, `thenQ`, `runQuery` |
| Storage | `createDatabase`, `openDatabase`, `closeDatabase`, `withDatabase`, `snapshot`, `transaction`, `commit`, `copyDatabase`, `backupBytes` |

Paths list arrows in traversal order; `Path "Task" []` is an identity. Arrows
are total functions. Use an incidence object for many-to-many or optional
relationships. Scalars are finite named elements, without a built-in type
algebra or SQL NULL. `edit` validates the final batch, so insertion order is
flexible. Deletion is restrictive; there is no implicit cascade.

Migration-generated `s0`/`p0` labels are deterministic within a result, **not
stable keys across edits**. Retain witnesses or domain keys. Relay Desk stores
original roster keys and explicit links, not these generated IDs.

`transaction` runs a pure callback under the connection lock. `Left` writes
nothing. Success appends and flushes one frame before acknowledgement. An
uncertain commit poisons the handle: reopen and inspect instead of blindly
retrying. Use `bracket` to close handles and share one handle among threads.
Other connections, including other processes, receive `Busy`.

An initial schema/instance snapshot is followed by diff frames; a schema change
writes a snapshot. Frames contain `CATTX001`, length/complement, Binary payload,
checksum and `COMMIT01`. Incomplete final frames are discarded on open;
complete corrupt frames fail closed. Frames are bounded at 256 MiB. Files can
be larger, but all logical data live in memory. The checksum is not authentication.

`copyDatabase` creates a new compact file without overwriting; `backupBytes`
returns an equivalent complete snapshot. These contain all private data.
POSIX directory metadata is not explicitly fsynced, so newly created filenames
have weaker power-loss guarantees than subsequent writes. Unusual/network
filesystems and actual power loss are not certified.

Relay Desk binds to `127.0.0.1`. GET `/api/state`, `/api/public`, `/api/backup`
return the workspace, handoff, and full backup. POST `/api/command` uses JSON
and `X-CatDB-Local: 1`; Host and browser Origin must match the loopback port.
There is no login, remote API or CORS. Imports are limited to 5 MiB and 5,000
rows per batch. Failed schema validation or query-budget checks save nothing.
