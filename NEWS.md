# citenets 0.3.0

## New functions

- `temporal_network()` — builds time-windowed networks with fixed, sliding, or
  cumulative strategies. Results include a `window` column for easy stacking.
- `historiograph()` — Garfield-style chronological citation network among the
  most locally cited documents.
- `local_citations()` — counts within-dataset citations (Local Citation Score).
- `backbone()` — Serrano et al. (2009) disparity filter for extracting
  statistically significant edges from dense weighted networks.
- `prune()` — threshold and top-n edge pruning.
- `read_biblio()` — universal reader with auto-format detection (Scopus, WoS,
  BibTeX, RIS, Dimensions, Lens.org).
- `read_dimensions()` — Dimensions CSV export reader.
- `read_crossref()` — converter for `rcrossref::cr_works()` output.
- `to_gephi()` — exports node and edge tables in Gephi CSV format; writes
  `nodes.csv` + `edges.csv` when a directory path is supplied.
- `to_graphml()` — pure base-R GraphML writer; no XML package required.
- `to_cograph()` — converts edge list to a `cograph_network` object with
  optional node metadata for direct use with `cograph::splot()`.

## Improvements

- All edge list functions now sort output by `weight` descending and reset
  row names.
- `local_citations()` canonical column order: `id`, `lcs`, `gcs`, `year`,
  `title`, `journal`, `doi`.
- `historiograph()` empty-result schema matches non-empty schema.
- All readers share a standard column order: `id`, `title`, `year`, `journal`,
  `doi`, `cited_by_count`, `abstract`, `type`, `authors`, `references`,
  `keywords`, then source-specific extras.
- `backbone()` and `prune()` use single-pass O(m) node statistics via
  `tapply()` / `split()` — faster on large networks.
- `temporal_network()` converted from `for` loop to `lapply`.
- `read_dimensions()` / `read_crossref()` now apply `standardize_authors()`
  and `standardize_refs()` for consistency with other readers.

# citenets 0.2.0

## Breaking changes

- Argument `count` renamed to `counting`; `measure` renamed to `similarity`
  across all network functions.
- `co_network()` renamed to `conetwork()`.

## New functions

- `read_openalex()` — reads OpenAlex JSON export.
- `filter_top()` — keeps only the top-n most connected nodes.
- `normalize()` — post-hoc normalisation of any edge list.

# citenets 0.1.0

Initial release.

- 8 network builders: `author_network()`, `document_network()`,
  `reference_network()`, `keyword_network()`, `institution_network()`,
  `country_network()`, `source_network()`, `conetwork()`.
- 13 counting methods including harmonic, geometric, golden ratio, adaptive
  geometric.
- 6 similarity normalisations: association strength, cosine, Jaccard,
  inclusion, equivalence.
- 6 readers: Scopus, Web of Science, OpenAlex, BibTeX, RIS, Lens.org.
- Converters: `to_igraph()`, `to_tbl_graph()`, `to_matrix()`.
- Numerically validated against bibliometrix and biblionetwork.
