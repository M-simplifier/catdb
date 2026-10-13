# PoC status

CatDB 0.2 is a working proof of concept, not a production-stable database or a
drop-in SQLite replacement. Relay Desk is a real local application using the
same library and native database file as the command-line tools.

Supported: finite acyclic schema presentations, path equations, finite-set
instances, Delta/Sigma/Pi migration with witnesses and universal factor maps,
atomic validated edits, one-file persistence, recovery of incomplete tails,
and compact backups. One process owns a database at a time; threads share one
connection. Data and queries live in memory.

Limits: no schema cycles, SQL, query optimizer, indexes on disk, network or
multi-process service, encryption, or automatic schema upgrades. The category
compiler allows at most 4,096 raw paths. Sigma allows 100,000 transported tokens
per target object. Pi allows at most 1,000,000 search
nodes and 100,000 result families per target object; exceeding a bound returns
an error instead of a partial result. Large files and migrations can exhaust
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
