# Native catalogs of changing data systems

`CatDB.Catalog` stores a finite collection of schemas, instances, schema mappings,
system correspondences and data-migration derivations in one CatDB instance.
The definitions are native rows and arrows in `catalogSchemaDef`, rather than
JSON documents stored in a value cell. `CatDB.Store` and its file protocol remain
unchanged. `combineInstances` forms a collision-checked disjoint union with an
application's instance; `projectInstance` and `projectCatalog` read its two parts.
Commit the combined instance in a single transaction.

The compiled `Catalog` is opaque. Insert schemas, instances and mappings under
fresh `SchemaKey`, `InstanceRev` and `MappingKey` values. An existing key cannot be
overwritten. A temporal predecessor and a revision reason are separate metadata;
they do not imply a natural transformation between the versions.

A `SystemMorphism` from `(C,I)` to `(D,J)` consists of `F:C→D` and
`I⇒Delta_F J`. Its constructor checks naturality and its composition implements
reindexing followed by natural-transformation composition. A Sigma unit gives
such a correspondence. A Pi counit points the other way, so a Pi derivation is
not silently registered as the same kind of system correspondence. Mappings
compare images as morphisms modulo the declared path equations, while retaining
the submitted path spellings in `mappingDef`.

`derive` records `PullAlong`, `PushAlong`, `JoinAlong` or `Generate` against a
fixed input revision and a fresh output revision. Each output row has a typed
witness: a direct pulled row, a Sigma equivalence class of transported tokens,
a Pi family indexed by its slots, or a generated-subinstance reference. These
references point into previous revisions, retaining operation order. A Pi family
with no slots is explicitly empty; it is not an observation. Generated labels
such as `p0` and `s0` identify rows only within their revision and object.

`bindAnchor` associates source rows with opaque external `AnchorId` values.
The application owns payload resolution and its factual/permission policy.
Derived rows cannot acquire direct anchor bindings; inspect them through
`explainRow` and the preceding witnesses. A Sigma class does not prove semantic
identity, and a Pi family does not prove truth or causation.

`adoptSystem` selects a revision for a named system. `adoptView` explicitly takes
`Active SystemKey` or `Pinned InstanceRev` and captures its source revision.
An active view becomes `Stale` when that source head changes. A pinned old view
is `PinnedSnapshot`. Any intermediate derived system may itself be a source.
No automatic recomputation or adoption occurs in this library.

Pass an explicit `Budget` to construction, migrations and catalog codecs.
Results are complete or fail with a typed `BudgetExceeded`; partial joins are
never successful results. Schema/path/equation work, materialized data, Sigma
tokens, Pi search/families and catalog replay are bounded. Sigma and Pi counters
are shared across the destination objects of one migration. These are operation
budgets, not a CPU-time guarantee. The legacy APIs use `defaultBudget`.

`encodeCatalog` is the persistence admission boundary. It checks the aggregate
retained derivation and witness counts, inner schemas and instances, correspondence
pullback sizes, migration footprints, and the materialized native record count.
It does not replay previously validated derivations. Pi search work is retained
in a private cache created only by `derive` or decoder replay; immutable inputs
keep that measured cost valid. This cache is not serialized and is reconstructed
when reading native rows. `checkReplayBudget` exposes the inexpensive replay
preflight; native record materialization is additionally checked by the encoder.
Validated opaque values retain `NFData` through explicit instances and do not
expose `Generic` reconstruction that could bypass their constructors.

For an opaque catalog admitted by `encodeCatalog b`, decoding its uncorrupted
result with the same budget and the same execution mechanism succeeds. A lower
encoding budget is also accepted when its actual requirements fit; it need not
dominate the budget originally used to derive the results. Insufficient budgets
fail before saving. The store does not persist an operational budget: opening
with a different, lower budget may fail explicitly, and the caller must retain
or supply a budget that admits the saved history. This is not a guarantee under
arbitrary budget changes or a CPU-time guarantee.

`maxCatalogReplays` and the aggregate record/witness bounds are capacities of the
whole retained catalog, rather than only one turn's work. A growing history can
reach them. The library reports the limit; it never silently removes history.

`decodeCatalog` and `projectCatalog` validate the dependent inner structures and
replay stored derivations, rejecting forged witnesses and orphaned native records. The current
format reproduces the current label enumeration exactly; a future enumeration
change needs an explicit format migration or an isomorphism-aware validator.
`Catalog`, its errors and the native data types have `NFData` instances so a host
can fully evaluate worker results before committing them.

The executable consumer examples and checks are in `test/NativeMemory.hs`.
They include a query whose two paths are equated: keeping every text and row
fixed but removing that equation changes Pi from two compatible rows to four.
The full Pi → generated roots → Delta → Sigma pipeline preserves its witnesses.
Other checks cover non-injective Sigma updates, interpretation corrections,
deletion, empty Pi families, correspondence laws, native persistence, stale heads,
budgets and atomic composition with an existing instance.
