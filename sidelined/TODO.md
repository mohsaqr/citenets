# Sidelined — abstract_network()

## What's here
- `R/abstract-network.R` — full implementation: greedy n-gram extractor, YAKE-lite scoring, AttentionRank-lite (freq / degree / PageRank / cosine cross-attention), UDPipe POS filtering, windowed co-occurrence with exponential decay
- `tests/test-abstract-network.R` — 39 passing tests
- `tmp/gen_wos_abstract.R` — WoS LA 1000-paper demo (9-tab scoring comparison)
- `tmp/gen_abstract_1000.R` — Scopus LA 1000-paper demo

## Why sidelined
Proximity-based within-abstract co-occurrence produces the wrong signal for a concept map.
The top edges reflect methodological boilerplate ("preferred reporting items" ↔ "systematic reviews")
and generic temporal phrases ("past decade" ↔ "field of education"), not substantive LA concepts.
All scoring variants (YAKE, AttentionRank × 4 cross-attention methods, count_weight scaling) produce
similar rankings because the architecture — not the scorer — is the bottleneck.

## What to rethink
The right design is document-level keyphrase co-occurrence:
1. Extract top-K keyphrases per abstract (YAKE or AttentionRank score)
2. Build edges between keyphrases that appear in the same abstract (not within-window proximity)
3. Weight by Jaccard / association across the corpus

This is equivalent to keyword co-occurrence but using extracted keyphrases instead of author keywords.
The attention/scoring logic here is reusable for step 1 (keyphrase selection per abstract).

## Do NOT merge back until the architecture is reconsidered
