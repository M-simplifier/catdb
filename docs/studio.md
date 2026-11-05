# CatDB Studio

CatDB Studio opens real `.catdb` files and lets you draw schemas, edit records, derive views and inspect their original-revision provenance. Its three workspaces are Schema Canvas, Instance Lens and Migration Canvas. Its interface is Japanese. The [Japanese guide](studio.ja.md) gives the full walkthrough.

Extract the Windows release and double-click **Start CatDB Studio.cmd**. Keep `studio-web` beside `catdb-studio.exe`. Your default browser opens the local application; the console also prints its launch URL. No Haskell installation or separate database service is needed. Close the console or press Ctrl+C to stop.

In **ファイルと書き出し** (Files/export), create a file or enter an existing file's full path. A filename without a directory is resolved under `data` beside the executable. The active absolute path is shown on that page. A new file or backup never replaces an existing file. Closing and reopening releases and reacquires the native lock. Drafts are browser memory; only applied revisions are saved.

Try **読書メモ** (reading notes). Schema Canvas shows the objects, generating arrows and declared equation. In Instance Lens, select `Note / n1`. Select the `book` edge label and change its target from `b1` to `b2`: the two declared-equal routes split. **下書きを確認** checks the whole draft in the real engine and rejects it. Change `quote` to `q2` as well, check again, then **まとめて適用** to commit both changes atomically. The original revision remains available. Fixture captions are fictional display labels; arbitrary records have IDs and total outgoing references, without a new scalar/string column system.

Create a Δ view with the checked `cards` mapping. Three notes remain three cards, including two cards referencing the same book. A result row's **由来** opens the source at the exact revision used. Editing the active source marks its existing view stale; **再計算する** recomputes explicitly. Old results remain in history.

Choose **対応の見本** (mapping example) for Σ and Π. Both use `by-key`, producing two declared quotient classes and two compatible families. Inspect `SigmaClass`, `PiFamily` (including an explicit empty family), `PullRef`, and `GeneratedRef` witnesses. Σ does not discover real-world identity; Π does not establish factual truth.

For your own data, use **空のスキーマ** (empty schema) in Schema Canvas. Add named objects, choose **射をつなぐ**, then select the source and target cards and name the new generator. Choose the common start, select path A or B, and click composable generators in order; the keyboard path builders offer the same choices. **定義を検査** checks the proposed definition in the native engine without writing it. Save is enabled after that check. Identity paths have no generators. Existing definitions can be copied under a new name with **別名で新しく作る**. Save under a fresh schema name, then add/edit/delete records in a draft. Deletion does not cascade: dangling references reject the whole draft. Saved schemas and revision snapshots are immutable. To change shape, add a new schema and a checked mapping. Historical or derived revisions can explicitly become the head of a named editable system.

Migration Canvas displays both actual schemas. **対象を対応させる** pairs a source object with a target object. Select a source generator, then click target generators to build its path image; the inspector also offers keyboard controls. Purple lines represent object mappings; selecting a source generator highlights the whole image path and its intermediate objects. **関手性を検査** checks the proposed mapping without saving it. The generic mapping editor assigns objects and sends each arrow to a composable path. Empty identity paths must be selected explicitly. The engine checks equations independently of today's rows. Δ uses `F: output-schema → input-schema`; Σ and Π use `F: input-schema → output-schema`. Views preserve their mapping, input revision and witnesses in the native catalog. Imported multi-step views remain inspectable; Studio refuses to silently replace their chain with its single-step recompute.

Legacy `.catdb` files without a catalog open read-only. Adopt them into a **new file**, preserving the original. Native catalogs combined with another application's data retain that outer data; editing a catalog snapshot does not mutate the other application's outer state.

Download a native backup or save one at a new path. Reopen the backup normally to restore it. Definition/data JSON can be reused through the [Haskell export example](../examples/StudioExport.hs). The native `.catdb` is canonical; Studio creates no authoritative JSON or SQLite sidecar.

Current limits: finite acyclic schemas, in-memory data, one exclusive native connection. Instance Lens draws at most 32 forward-reachable elements from the selected row; selecting another node refocuses the neighborhood. Expand **表・検索・行の追加を開く** for the table and record controls. Drag nodes to retain manual display positions, drag the background to pan, or use fit/zoom/rearrange and keyboard buttons. These positions are browser presentation state, not database schema data. The record table searches all rows and displays the first 100 matches; rule lists display the first 200 matches while counting all rows. Every save checks the entire draft. Core `defaultBudget` bounds computation and retained history; pure worker computation also has a 20-second timeout and a cancel action. A disk commit already underway cannot be cancelled. No SQL, NULL, general scalar algebra, recursive schemas, indexes, optimizer or automatic schema evolution. See [STATUS](../STATUS.md).

For developers, `build.ps1 -Test` builds the native executables; `build-frontend.ps1` uses the pinned Miso/Nix/GHC-WASM toolchain in Ubuntu WSL. On Linux, run `cabal build all` and, from `studio/frontend`, `nix develop .#wasm --command bash ./build.sh`. Start `catdb-studio --assets studio/web --data-dir ./data`. The native backend is Servant/Warp; model, update and views are Miso Haskell. JavaScript handles only WASM loading, session handoff and browser downloads.

Before preparing a binary release, regenerate frontend notices with
`python scripts/collect_wasm_notices.py --plan studio/frontend/dist-newstyle/cache/plan.json --cache build/notice-cache --output studio/frontend/static/licenses`,
then copy those notices into the built web output (or rerun the frontend build).
`package-studio.ps1 -GhcSourceArchive PATH/ghc-9.6.7-src.tar.xz` prepares a fresh
Windows zip and corresponding-source archive from `build/`. It requires the
matching official GHC source archive and the native Hackage source cache, and
never replaces an existing package. The package includes the licenses and the
GMP modification/recombination instructions. No push or release is performed.
