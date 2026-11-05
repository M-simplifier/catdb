# Official CatDB website

The English and Japanese pages share the adopted product design and equivalent
product facts. They introduce schemas, checked mappings, Δ/Σ/Π and the real
CatDB Studio, then give its finite-acyclic, total-arrow and primitive-column
limits. Contributions go through issues; the project does not accept PRs.

`assets/studio-graph.png` is an actual Studio capture with fictional reading
notes. It must never be replaced with a mock presented as an application image.
The interactive examples in `site.js` are explicitly explanatory simulations;
they do not run the database engine in the page.

All styles and scripts are local, with deliberate system font fallbacks and
no CDN, analytics, credentials or live user data. Canonical, language and Open
Graph metadata target GitHub Pages. Windows links target v0.4.0-poc and must be
published with that release. The existing workflow checks `site/site.js` and
publishes this directory after the native build and HTTP checks.
