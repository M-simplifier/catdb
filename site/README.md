# Official website

Buildless HTML, CSS and JavaScript. GitHub Pages publishes this directory through
the existing CI workflow. English lives at `index.html`; Japanese at
`ja/index.html`. Both use `style.css` and `site.js`. Each page has its own copy,
headings and language metadata. Keep the language switch explicit; do not redirect
visitors based on their browser language.

## Interactive example

The homepage replays observed Relay Desk v0.2.0-poc results. It does **not** run
CatDB in the browser. The released Haskell app and library perform the actual
computations and persistence. The illustrated names and addresses are fictional.

With a new `--demo` workspace:

- Initial state: 6 roster records, 5 resolved people, 7 eligible request–person
  pairs. REQ-102 (Engineering) has Jon Bell and Jon B. as its 2 candidates.
- Link `Team directory/2` to `Contractors/2`: still 6 records, now 4 people and
  6 eligible pairs. REQ-102 has 1 candidate, Jon Bell. Removing the link restores
  the original counts and source records.
- Assign REQ-101 (Design) to Maya. Trying to replace her with Jon Bell
  (Engineering) is rejected; the saved assignment remains Maya.

The page uses these recorded before/after states, not timings or estimates.
Identity and membership policy live in `reference/src/Relay/Model.hs`; mutations
are handled by `reference/src/Main.hs`. The homepage links to the original
release-baseline schema and mapping test for inspectable evidence.

`assets/relay-desk.jpg` is a real app screenshot from a separate session with
7 imported roster records. Its caption distinguishes that session from the
6-record interactive example.

## Editing

Keep the shared IDs for the two demonstrations and copy button in both languages.
Update the copy in `site.js` alongside the static initial HTML. Japanese headings
and text measure are intentionally different from English; write for the reader
rather than translating line by line.

The site has no runtime dependencies, analytics, remote fonts, or build step.
Run `node --check site/site.js` and preview both languages at desktop and mobile
widths when changing interactions or layout. Preserve the PoC, MIT and issue-only
contribution contract in both languages.
