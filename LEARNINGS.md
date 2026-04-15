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
- [biblionetwork coupling]: Uses Salton's cosine (coupling angle) by default, which equals `document_network(d, "coupling", measure = "cosine")` in citenets.
