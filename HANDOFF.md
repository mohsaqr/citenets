# Session Handoff — 2026-04-19

## Completed

- **Author network attention** (`R/author-network.R`): `attention` parameter with four modes:
  - `"proximity"` — middle authors weighted most via `pmin(k-1, n-k) / floor(n/2)`
  - `"lead"` — first author dominant
  - `"last"` — last author (PI) dominant
  - `"none"` — full counting, no attention weighting
  - Demo at `tmp/gen_la_proximity.R` → `tmp/la_proximity.html` (tabbed 4-way comparison)

- **`abstract_network()` — sidelined** (`sidelined/`):
  - Full implementation moved out of package (not exported, not tested in main suite)
  - See `sidelined/TODO.md` for rationale and what to rethink
  - UDPipe POS filtering, AttentionRank-lite scoring, windowed co-occurrence all implemented and working — the architecture (proximity-within-abstract) is the problem, not the code

- **Package hygiene**: trailing comma fixed in DESCRIPTION, orphaned `man/abstract_network.Rd` removed, NAMESPACE clean, 640 tests passing

## Current State

- Package: v0.3.0, 640 tests, 0 failures
- All eight network builders work: `author_network`, `keyword_network`, `reference_network`, `document_network`, `source_network`, `country_network`, `institution_network`, `conetwork`
- Author network has new `attention` parameter
- `abstract_network` parked in `sidelined/` — do not ship

## Key Decisions

- **Attention for authors, not abstracts**: position-based attention makes sense for bylines (first/middle/last author roles are meaningful); it does not translate to continuous text where token position is arbitrary
- **`abstract_network` architecture problem**: proximity within one abstract is too noisy — a 150-word abstract puts almost everything within a 10-token window. The right design is document-level keyphrase co-occurrence: top-K phrases per abstract → edge if both appear in same paper → weight by Jaccard/association. Analogous to keyword co-occurrence but using extracted keyphrases.
- **Sidelined, not deleted**: UDPipe path, AttentionRank-lite, greedy n-gram extractor, 39 tests all preserved and working in `sidelined/`

## Open Issues

- `abstract_network` needs redesign around document-level co-occurrence (not proximity-based)
- AttentionRank scoring in `sidelined/` is reusable for keyphrase selection step
- `sidelined/TODO.md` has full design notes

## Next Steps

1. **Redesign `abstract_network`** using document-level co-occurrence:
   - Per abstract: score all phrases → keep top K by score (reuse YAKE/AttentionRank from `sidelined/`)
   - Corpus level: edge(A, B) = abstracts containing both A and B
   - Normalize with Jaccard/association (reuse `R/normalize.R`)
2. Test with WoS LA corpus — expect substantive LA concepts, not methodological boilerplate
3. Expose `top_k` and `similarity` as parameters

## Context

- WoS LA data: `/Users/mohammedsaqr/Downloads/LA_bibliometric_data 2/WoS/savedrecs (2–6).xls` — 4264 papers, 4162 with abstracts
- UDPipe model cached at `~/Library/Application Support/org.R-project.R/R/bibnets/english-ewt-ud-2.5-191206.udpipe`
- `devtools::load_all(".")` then `devtools::test(".")` to verify state
