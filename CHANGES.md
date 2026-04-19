### 2026-04-18 — Bipartite deduplication fix + equivalence validation
- R/bipartite.R: Fixed `build_bipartite()` — deduplicate `(work_idx, entity_names)` pairs before constructing the sparse matrix. `Matrix::sparseMatrix` sums duplicate indices, so an author listed twice in one paper inflated co-authorship weights (e.g. 4× instead of 1). 7 papers in the OpenAlex dataset triggered this.
- Equivalence confirmed on `open_alex_gold_open_access_learning_analytics` (1,508 papers): full-counting co-authorship = biblionetwork (max|diff|=0, 12,270 edges); fractional counting = biblionetwork (max|diff|=4.4e-16, floating point only); bibliographic coupling + cosine = biblionetwork (max|diff|=0, 36 edges).
- Tests: 560 passing, 0 failing.

### 2026-04-18 — OpenAlex flat CSV reader
- R/read-openalex.R: Added `read_openalex_csv()` — reads the flat CSV format downloaded from openalex.org (pipe-delimited multi-value fields). Maps to standard bibnets columns; `references` and `abstract` are always empty/NA (not available in flat export).
- R/read-biblio.R: Added `"openalex_csv"` dispatch in `read_single_biblio()`; `detect_format()` detects via `authorships.author.display_name` header. Updated `format` parameter docs.
- R/utils.R: Fixed `split_field()` to filter empty elements after splitting — prevents blank-label nodes from trailing/duplicate separators.
- inst/extdata/openalex_works.csv: Re-filtered to 733 rows (was 704) — now excludes both NAs and empty strings in key fields.
- tests/testthat/test-read-openalex-csv.R: 24 tests covering column structure, URL stripping, list-column splitting, auto-detection, error handling.
- Tests: 560 passing, 0 failing.

### 2026-04-18 — Formula fixes and cross-package validation
- R/counting.R: Fixed `counting = "strength"` formula to match Perianes-Rodriguez (2016). Was `Σ IDF(r) / √(n_i × n_j)` (geometric mean denominator); now `Σ IDF(r) / (n_i × n_j)` (product denominator). Row normalization deferred to multiply_bipartite via `attr(B, "row_scale")`.
- R/counting.R: Fixed all non-full counting methods (`fractional`, `paper`, `strength`) dropping row names on `Diagonal() %*% B`. Now saves/restores `dimnames(B)`. Affected `document_network("coupling")` which was returning integer indices as node names.
- R/multiply.R: Added `row_scale` post-divide step for `counting = "strength"` when `similarity = "none"`.
- R/methods.R: Fixed `summary.bibnets_network()` crash on empty networks (0 edges).
- R/read-biblio.R: Updated error message for unsupported formats (OpenAlex/Crossref require pre-loading).
- DESCRIPTION: Corrected reader capabilities.
- tests/testthat/test-equiv-validation.R: New 66-test cross-package validation suite comparing bibnets against biblionetwork (full/fractional/paper/cosine/coupling/co-citation/strength) and bibliometrix (coupling/co-citation cosine) across 6 synthetic datasets.
- Tests: 536 passing, 0 failing.

### 2026-04-18 — Package rename: citenets → bibnets
- DESCRIPTION: Package name changed to `bibnets`, URLs updated
- R/methods.R: `citenets_network` S3 class → `bibnets_network`; `as_citenets_network()` → `as_bibnets_network()`
- All R source, man/, tests/, vignettes/, tutorials/: global `citenets` → `bibnets` substitution
- vignettes/citenets.Rmd → vignettes/bibnets.Rmd
- tutorials/citenets-tutorial.qmd → tutorials/bibnets-tutorial.qmd
- man/print.citenets_network.Rd → man/print.bibnets_network.Rd
- man/summary.citenets_network.Rd → man/summary.bibnets_network.Rd
- Tests: 497 passing, 0 failing

### 2026-04-16 — Exhaustive function revision
- R/methods.R: `as_bibnets_network()` now handles `format` conversion (gephi/igraph/cograph/matrix/edgelist)
- R/utils.R: Added 5 validation helpers (check_data, check_edges, check_choice, check_format, check_file)
- R/multiply.R: Removed redundant `shared` column (was always == `count`)
- R/bipartite.R: Auto-splits non-list columns via `ensure_list_column()`
- R/{all 8 network builders}: Added `format` param, human-readable errors, consistent `@return` docs
- R/{backbone,prune,filter}.R: Human-readable error messages
- R/{historiograph,normalize,temporal-network}.R: Human-readable errors; temporal generalized with `time_col`
- R/read-*.R (all 8): `check_file()`, consistent @return docs, encoding param on bibtex/ris/lens
- R/read-biblio.R: Fixed `detect_format()` for Dimensions metadata header
- R/keyword-network.R, reference-network.R: Default threshold changed from 1 to 0
- R/filter.R: Fixed missing rowname reset
- R/data.R: Fixed stale example function names
- .Rbuildignore: Added LA_bibliometric_data
- Tests: 497 passing (2 fewer from removed `shared` assertions), 0 failing
- R CMD check: 0/0/0

### 2026-04-16 — Production-readiness pass (v0.3.0, part 2)
- R/source-network.R, country-network.R, institution-network.R, co-network.R: Wired `as_bibnets_network()` into all 4 remaining network builders; now all 8 return a stamped `bibnets_network` S3 object
- tests/testthat/test-backbone.R: 8 tests for Serrano disparity filter including alpha value correctness, degree-1 node guarantee, sort order
- tests/testthat/test-prune.R: 9 tests for threshold and top_n pruning with column preservation
- tests/testthat/test-historiograph.R: 12 tests for local_citations() (LCS counts, metadata columns) and historiograph() (top-n selection, edge structure, min_lcs filter)
- tests/testthat/test-temporal-network.R: 10 tests for temporal_network() and build_windows() — all three strategies, string name resolution, year range correctness
- tests/testthat/test-filter.R: 6 tests for filter_top()
- tests/testthat/test-converters.R: 17 tests for to_gephi(), to_graphml(), to_matrix() — file writing, column names, XML content, symmetry
- tests/testthat/test-remaining-networks.R: 21 tests for source/country/institution/conetwork — S3 class, correct nodes, attribute metadata
- tests/testthat/test-methods.R: 11 tests for bibnets_network S3 methods, attribute stamping, print/summary output
- vignettes/bibnets.Rmd: Getting started vignette covering all major features
- .github/workflows/R-CMD-check.yml: CI on macOS/Windows/Ubuntu × R release/devel/oldrel-1
- README.md: Updated with v0.3.0 features, comparison table, badges, references
- DESCRIPTION: Added bibliometrix, knitr, rmarkdown to Suggests; added VignetteBuilder: knitr
- .Rbuildignore: Added .claude directory
- R/methods.R: Added @importFrom utils head to suppress R CMD check NOTE
- Tests: 499 passing, 0 failing

### 2026-04-15 — Initial package build (v0.1.0)
- DESCRIPTION, LICENSE, NAMESPACE: Package skeleton with MIT license, R >= 4.1.0, Matrix dep
- R/bipartite.R: Two-mode matrix construction engine (Batagelj & Cerinsek 2013)
- R/counting.R: 13 counting methods — full, fractional, paper, strength, harmonic, arithmetic, geometric, adaptive_geometric, golden, first, last, first_last, position_weighted
- R/normalize.R: 6 normalization methods — association, cosine, jaccard, inclusion, equivalence, none
- R/multiply.R: Matrix multiplication engine with raw count tracking
- R/edgelist.R: Matrix to/from edge list conversion
- R/author-network.R: author_network() — collaboration, coupling, co_citation, equivalence types
- R/document-network.R: document_network() — coupling, citation, co_citation, equivalence
- R/reference-network.R: reference_network() — co_citation, equivalence
- R/keyword-network.R: keyword_network() — co_occurrence
- R/institution-network.R: institution_network() — collaboration, coupling, equivalence
- R/country-network.R: country_network() — collaboration, coupling, equivalence
- R/source-network.R: source_network() — coupling, co_citation, equivalence
- R/converters.R: to_igraph(), to_tbl_graph(), to_matrix()
- R/utils.R: split_field(), aggregate_by_entity(), standardize_authors/refs()
- R/read-*.R: Readers for Scopus, WoS, OpenAlex, BibTeX, RIS, Lens.org
- R/data.R: biblio_data example dataset (10 papers, 6 authors, 10 refs, 24 keywords)
- Tests: 11 test files — unit tests + equivalence tests vs bibliometrix and biblionetwork
