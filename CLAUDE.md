# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`bibnets` is an R package (v0.3.0) for constructing bibliometric networks from scholarly data. It reads Scopus, WoS, OpenAlex, BibTeX, RIS, Lens.org, Dimensions, and Crossref exports and produces co-citation, coupling, co-authorship, keyword co-occurrence, and direct citation networks.

- **CRAN dependencies**: `Matrix`, `stats`, `utils` only — keep it that way.
- **Suggested**: `igraph`, `tidygraph`, `cograph`, `bibliometrix`, `rcrossref`, `openalexR`.
- **R >= 4.1.0** required (base pipe `|>` used throughout).

## Common Commands

```bash
# Load package in-development
Rscript -e 'devtools::load_all(".")'

# Run all tests
Rscript -e 'devtools::test(".")'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-author-network.R")'

# Build documentation
Rscript -e 'devtools::document(".")'

# Quick R CMD check (no tests, no examples, no vignettes)
Rscript -e 'devtools::check(".", args = c("--no-tests", "--no-examples", "--no-vignettes", "--no-manual"))'

# Full check
Rscript -e 'devtools::check(".")'
```

## Architecture

### Core computation pipeline

Every network builder follows this exact sequence:

```
build_bipartite()  →  apply_counting()  →  multiply_bipartite()  →  as_bibnets_network()
   (R/bipartite.R)      (R/counting.R)       (R/multiply.R)            (R/methods.R)
```

1. **`build_bipartite(data, field)`** — converts a list-column into a sparse works × entities `dgCMatrix`. Normalizes all entity labels to `toupper(trimws(...))`. Calls `ensure_list_column()` first so semicolon strings are auto-split.

2. **`apply_counting(B, counting, network_type)`** — weights the binary matrix using one of 13 counting methods (in `R/counting.R`). Position-dependent methods (harmonic, arithmetic, geometric, etc.) use `build_author_bipartite()` instead of the generic path.

3. **`multiply_bipartite(B, mode, similarity, threshold, top_n)`** — computes `crossprod(B)` (columns mode) or `tcrossprod(B)` (rows mode) entirely in sparse representation, then optionally normalizes with `normalize()` (`R/normalize.R`). Extracts upper-triangle edges without ever creating a dense matrix.

4. **`as_bibnets_network(edges, ..., format)`** in `R/methods.R` — the sole output format dispatcher. Stamps the result as `bibnets_network` S3 class (edgelist default), or converts to Gephi CSV, igraph, cograph, or sparse matrix. **Adding a new output format only requires editing this one function.**

### Network builders (`R/*-network.R`)

Eight exported builders: `author_network`, `keyword_network`, `reference_network`, `document_network`, `source_network`, `country_network`, `institution_network`, `conetwork`. All share the same parameter signature: `type`, `counting`, `similarity`, `threshold`, `top_n`, `format`.

- Coupling networks use `mode = "rows"` (row-mode multiplication).
- Co-occurrence / co-citation networks use `mode = "columns"`.
- `conetwork` (via `build_by_network`) supports arbitrary `field` + `by` combinations.

### Standard output schema

All network functions return a `bibnets_network` data frame with columns: `from`, `to`, `weight`, `count`. Attributes: `network_type`, `counting`, `similarity`.

### Readers (`R/read-*.R`)

- `read_biblio()` is the unified entry point — handles single files, multiple files, and directories. Auto-detects format via `detect_format()`.
- **Dimensions quirk**: CSV starts with a `"About the data: ..."` metadata line. `detect_format()` checks line 2 when line 1 matches that pattern.
- Standard output column order: `id`, `title`, `year`, `journal`, `doi`, `cited_by_count`, `abstract`, `type`, then list-columns `authors`, `references`, `keywords`, then source-specific extras.

### Validation helpers (`R/utils.R`)

All user-facing functions use `check_data()`, `check_edges()`, `check_choice()`, `check_format()`, `check_file()` — never raw `stopifnot()`. These produce actionable error messages.

### Post-processing

- `backbone(edges, alpha)` — Serrano et al. (2009) disparity filter.
- `prune(edges, ...)` / `filter_top(edges, ...)` — threshold and top-n pruning.
- `temporal_network(data, network_fun, ..., strategy)` — wraps any builder with fixed/sliding/cumulative time windows.
- `local_citations()` + `historiograph()` — LCS computation and historiograph construction.
- Converters: `to_igraph()`, `to_tbl_graph()`, `to_matrix()`, `to_gephi()`, `to_graphml()`, `to_cograph()`.

## Key Invariants

- **Entity labels are always uppercased** — `build_bipartite()` calls `toupper(trimws(...))` unconditionally. Tests comparing node labels must use uppercase (e.g., `"ALICE"` not `"Alice"`).
- **`backbone()` alpha is strictly (0, 1)** — do not use `alpha = 1` in tests; use `0.9999`.
- **`count` column ≠ `weight`** — `count` is raw binary co-occurrence; `weight` is the (possibly normalized) similarity score.
- **No `shared` column** — was removed in v0.3.0; output is `from`, `to`, `weight`, `count` only.
- **`.claude/` in `.Rbuildignore`** — the `.claude` directory triggers an R CMD check NOTE if not listed there.

## Test Files

| File | Coverage |
|------|----------|
| `test-author-network.R` | author_network() all types + counting methods |
| `test-backbone.R` | Serrano disparity filter |
| `test-bipartite.R` | build_bipartite(), build_bipartite_long() |
| `test-converters.R` | to_gephi(), to_graphml(), to_matrix() |
| `test-counting.R` | all 13 counting methods |
| `test-document-network.R` | document_network() |
| `test-equiv-bibliometrix.R` | numerical equivalence vs bibliometrix |
| `test-equiv-biblionetwork.R` | numerical equivalence vs biblionetwork |
| `test-equiv-counting.R` | counting method equivalence |
| `test-filter.R` | filter_top() |
| `test-historiograph.R` | local_citations(), historiograph() |
| `test-keyword-network.R` | keyword_network() |
| `test-methods.R` | bibnets_network S3 methods, print/summary |
| `test-normalize.R` | 6 normalization methods |
| `test-prune.R` | prune(), threshold and top_n |
| `test-reference-network.R` | reference_network() |
| `test-remaining-networks.R` | source/country/institution/conetwork |
| `test-temporal-network.R` | temporal_network(), build_windows() |

**Current status**: 499 tests passing, 0 failing.
