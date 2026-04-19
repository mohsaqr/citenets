### 2026-04-18 (equivalence-real-dataset session)
- [build_bipartite duplicate entity entries]: `Matrix::sparseMatrix(i, j, x=1)` **sums** duplicate (i,j) pairs — if an author appears twice in one paper's author list, `B[paper, author]=2`, and `crossprod(B)` inflates co-authorship weights multiplicatively (2×2=4 instead of 1). Fixed by deduplicating `(work_idx, entity_names)` pairs before sparseMatrix call. 7 papers in the OpenAlex dataset had this issue.
- [equivalence confirmed]: bibnets full-counting co-authorship = biblionetwork full_counting (max|diff|=0). Fractional counting: max|diff|=4.4e-16 (floating point only). Bibliographic coupling + cosine: max|diff|=0. 12,270 edges all match.
- [keyword co-occurrence needs ≥2 keywords per paper]: `keyword_network` returns 0 edges when each paper has exactly 1 keyword (primary_topic only from flat CSV). Cannot validate keyword co-occurrence against biblionetwork with this dataset.
- [biblionetwork biblio_coupling default = cosine]: `biblionetwork::biblio_coupling` normalizes by cosine by default. bibnets equivalent: `document_network(data, "coupling", counting="full", similarity="cosine")`.

### 2026-04-18 (openalex-csv session)
- [OpenAlex flat CSV vs nested]: Flat CSV export from openalex.org uses dot-notation column names (`authorships.author.display_name`) and pipe (`|`) as multi-value separator. Distinct from `openalexR::oa_fetch()` output (nested list-columns). Detection fingerprint: `authorships.author.display_name` in header.
- [split_field empty elements]: `split_field()` was not filtering empty strings after splitting — trailing/duplicate separators (e.g. "CZ||GB") produced empty-string actor nodes. Fixed by `parts[nchar(parts) > 0]`. Safe change: empty actors are never valid network nodes.
- [OpenAlex flat CSV no references]: The flat export has no references/cited_works column — only `cited_by_count`. Cannot build reference-based networks (coupling, co-citation) from this format.
- [Dataset filter: NA + empty string]: filter_to_complete = no NAs AND no empty strings (`trimws(x) != ""`). 1,533 rows → 733 rows for the learning analytics OpenAlex dataset.

### 2026-04-18 (equiv-validation session)
- [strength counting formula bug]: `counting = "strength"` was computing `Σ IDF(r) / √(n_i × n_j)` — Salton's cosine denominator with IDF numerator — NOT the Perianes-Rodriguez (2016) formula `Σ IDF(r) / (n_i × n_j)`. Root cause: `Diagonal(sqrt(row_w)) %*% B %*% Diagonal(sqrt(idf))` symmetrically halves the row normalization, giving geometric-mean denominator. Fix: IDF applied to columns only in `apply_counting`; row normalization deferred to `multiply_bipartite` as a post-divide step via `attr(B, "row_scale")`.
- [Diagonal %*% B drops dimnames]: `Matrix::Diagonal(x) %*% B` silently drops the row names of B. Must save/restore `dimnames(B)` around any diagonal multiplication in `apply_counting`. Affected `fractional`, `paper`, AND `strength` for coupling networks — causing integer indices instead of paper IDs as node names.
- [biblionetwork normalizeSimilarity "cos" invalid]: `bibliometrix::normalizeSimilarity(M, type="cos")` fails silently (S not found). Valid type for cosine is `"salton"`.
- [bibliometrix coupling rownames]: `biblioNetwork(bm, "coupling")` returns papers labeled "1 NA 1", "2 NA 2" (position-based). Rename `rownames(M) <- bm$UT` to align with bibnets paper IDs.
- [bibliometrix co-citation node names]: Co-citation node names are truncated to "AUTHOR YEAR" format by `labelShort()`. Bibnets preserves the full WoS string "AUTHOR, YEAR, JOURNAL". Align with `sub("^([^,]+), *([0-9]{4}).*", "\\1 \\2", x)`.
- [data.table scoping in testthat]: `dt[!is.na(entity)]` fails in devtools::test() — column names not in scope inside helper functions. Filter on base data.frame before `as.data.table()`.
- [single-author paper denominator divergence]: `biblionetwork::coauth_network` filters `nb_auth > 1` before computing each author's paper count for cosine normalization. bibnets includes all papers in `diag(C)`. Ensure all synthetic test papers have ≥2 authors to match.

### 2026-04-18
- [author_weights dead code]: The `fractional` and `paper` cases in `author_weights()` are never called via the public API (the function is only invoked from `build_author_bipartite()` which is only called for positional methods). `fractional = rep(1/n, n)` is correct as a per-author weight and is directly tested. `paper = rep(1/n, n)` was wrong (comment claimed `sqrt(2/(n*(n-1)))`) — removed it. Correct Batagelj paper formula lives in `apply_counting()`.
- [normalize dual path]: `normalize()` has two code paths: dense (`as.matrix()`) for < 500 nodes, sparse triplet for >= 500. Both are correct but the dense path allocates a full matrix.
- [two counting formulas for fractional]: `apply_counting(, "fractional", "symmetric")` uses Perianes-Rodriguez `1/(n-1)` per shared paper. `author_weights(n, "fractional")` returns `1/n` (simple per-author weight). These are different and both intentional — do not confuse them.
- [package rename]: citenets → bibnets. S3 class `citenets_network` → `bibnets_network`. All man/, tests/, tutorials/, vignettes/ updated via global perl substitution.

### 2026-04-16 (revision pass)
- [shared column redundant]: `count` and `shared` columns in `multiply_bipartite` output were always identical. Both came from `A_raw[cbind(i,j)]`. Removed `shared` — output is now `from`, `to`, `weight`, `count`.
- [format parameter]: `as_bibnets_network()` is the single choke point for output format. Adding `format` there gives all 8 builders `gephi`/`igraph`/`cograph`/`matrix` output for free.
- [detect_format Dimensions]: Dimensions CSVs start with `"About the data: ..."` metadata on line 1. `detect_format()` must check line 2 when line 1 matches `^"?About the data`.
- [auto-split non-list columns]: `build_bipartite()` now calls `ensure_list_column()` so users can pass semicolon-delimited strings directly without pre-splitting. Removes the cryptic `is.list() is not TRUE` error.
- [stopifnot user-facing]: `stopifnot()` produces messages like `"id" %in% names(data) is not TRUE` — unusable for end users. Replaced with `check_data()`, `check_edges()`, `check_choice()`, `check_format()`, `check_file()` helpers that produce actionable messages.
- [gephi empty edge list]: `$<-` on a 0-row data.frame with a scalar value errors (`replacement has 1 row, data has 0`). Use `rep(value, nrow(out))` instead.
- [keyword_network threshold]: Was `threshold = 1` while all other builders defaulted to `0`. Changed to `0` for consistency.

### 2026-04-16 (continued)
- [build_bipartite uppercasing]: All entity names go through `toupper(trimws(...))` in build_bipartite. Tests comparing node labels must use uppercase (e.g., "ML" not "ml", "OXFORD" not "Oxford"). This is intentional normalization.
- [backbone alpha bounds]: `backbone()` validates `alpha > 0 && alpha < 1` strictly. Tests cannot use `alpha=1` to "keep everything" — use 0.9999 instead.
- [expect_s3_class info arg]: `testthat::expect_s3_class()` does not accept an `info=` argument. Use `expect_true(inherits(x, "cls"))` when a custom failure message is needed.
- [temporal_network empty windows]: When `threshold` is very high, `nrow(edges) == 0` triggers the `else NULL` branch, windows are filtered out, and the result is an empty list. A for loop over `names(tn)` is an empty test — check `length(tn) == 0` instead.
- [conetwork build_by_network]: The `field` entities (what nodes are) are NOT uppercased by `build_by_network` since it goes through a merge/aggregate path before `build_bipartite`. But the final `build_bipartite` call in that function uppercases the BY-values (the linking field), not the entity names. So in `conetwork(d, "authors", by="keywords")`, author names preserve their original case, but keyword values get uppercased internally.
- [R CMD check .claude dir]: The `.claude` directory in project roots triggers an R CMD check NOTE. Must be added to `.Rbuildignore` as `^\.claude$`.

### 2026-04-16
- [output standardization]: Standard column order for all readers is: `id`, `title`, `year`, `journal`, `doi`, `cited_by_count`, `abstract`, `type`, then list-columns `authors`, `references`, `keywords`, then source-specific extras. Extras include `index_keywords`, `affiliations`, `language` (Scopus), `keywords_plus` (WoS), `affiliations`/`countries` (Dimensions).
- [empty result schema]: `empty_biblio_df()` now includes `authors`, `references`, `keywords` list-columns so empty and non-empty results share the same schema.
- [temporal_network window column]: Each edge data.frame in the temporal_network list now has a `window` column so `do.call(rbind, result)` is immediately interpretable.
- [local_citations column order]: Output order is `id`, `lcs`, `gcs` (if cited_by_count present), `year`, `title`, `journal`, `doi`. Year is always coerced to integer.
- [backbone/prune sorting]: Both now sort by `weight` descending after filtering and reset rownames.

### 2026-04-15
- [bibliometrix cocMatrix]: Simple ref strings like "REF1" produce empty matrices. Must use WoS-style "AUTHOR YEAR JOURNAL" format for cocMatrix to parse correctly.
- [bibliometrix biblioNetwork]: Requires `DB` column in data frame, crashes without it.
- [bibliometrix normalizeSimilarity]: Uses diagonal of co-occurrence matrix as total counts. Formula implementations match the van Eck & Waltman (2009) definitions exactly.
- [matrix multiplication counting]: For position-dependent counting (harmonic etc.), bipartite entry = positional weight. crossprod then gives link weight = w_i * w_j, which is the natural extension of positional credit to network edges.
- [fractional vs paper counting]: `fractional` (1/n per entity) and `paper` (total per paper = 1, i.e., 2/(n*(n-1)) per link for symmetric) are different. The former normalizes per entity, the latter per paper.
- [biblionetwork coupling]: Uses Salton's cosine (coupling angle) by default, which equals `document_network(d, "coupling", measure = "cosine")` in bibnets.
