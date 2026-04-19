# bibnets

[![R-CMD-check](https://github.com/mohsaqr/bibnets/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/mohsaqr/bibnets/actions/workflows/R-CMD-check.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

R package for constructing bibliometric networks from scholarly data — co-authorship, co-citation, bibliographic coupling, keyword co-occurrence, historiographs, and more.

## What it does

- **8 network functions**: author, document, reference, keyword, source, institution, country, plus generic `conetwork()`
- **13 counting methods**: full, fractional, paper, strength, harmonic, arithmetic, geometric, adaptive geometric, golden ratio, first, last, first–last, position-weighted
- **4 attention weights** (`attention =`): proximity, lead, last, circular — position-based weighting inspired by attention mechanisms in TNA, applicable to author, keyword, country, and institution networks
- **6 similarity measures**: association strength, cosine, Jaccard, inclusion, equivalence
- **9 readers**: Scopus, Web of Science, OpenAlex (JSON + flat CSV), BibTeX, RIS, Lens.org, Dimensions, Crossref (rcrossref)
- **Auto-detect reader**: `read_biblio()` identifies format from file content
- **Network reduction**: `backbone()` (Serrano disparity filter), `prune()`, `filter_top()`
- **Temporal analysis**: `temporal_network()` — fixed, sliding, or cumulative windows over any network function
- **Historiograph**: `historiograph()` + `local_citations()` — chronological citation history
- **Export**: `to_gephi()`, `to_graphml()`, `to_igraph()`, `to_matrix()`, `to_cograph()`
- **S3 class**: all network functions return a `bibnets_network` with `print()` and `summary()` methods
- **3 hard dependencies** (Matrix, stats, utils)
- Numerically validated against bibliometrix and biblionetwork

## Install

```r
# install.packages("remotes")
remotes::install_github("mohsaqr/bibnets")
```

## Quick start

```r
library(bibnets)

# Auto-detect format and read
data <- read_biblio("export.csv")      # Scopus, WoS, Dimensions, Lens, BibTeX, RIS, OpenAlex CSV

# Build networks
author_network(data, "collaboration", counting = "harmonic")
reference_network(data, similarity = "association")
document_network(data, "coupling", similarity = "cosine")
keyword_network(data)

# Attention weighting — independent of counting/type
author_network(data, attention = "lead")        # first author dominates
author_network(data, attention = "last")        # last author (PI/supervisor) dominates
author_network(data, attention = "proximity")   # center-of-list authors weighted most
author_network(data, attention = "circular")    # first + last both prominent
keyword_network(data, attention = "lead")       # works on any network type
country_network(data, attention = "circular")

# Generic: works with any data frame and any column
conetwork(data, "authors", by = "keywords")   # authors linked by shared keywords
conetwork(df, "Authors", sep = ";")           # auto-splits delimited strings

# Reduce
edges <- author_network(data, "collaboration")
backbone(edges, alpha = 0.05)                 # Serrano disparity filter
prune(edges, method = "top_n", value = 3)     # top-3 neighbours per node

# Temporal
temporal_network(data, keyword_network, window = 3)
temporal_network(data, author_network, "collaboration",
                 window = 2, strategy = "sliding")

# Historiograph
h <- historiograph(data, n = 30)
h$nodes   # top-cited documents
h$edges   # directed citation links with year metadata

# Export
to_gephi(edges)                               # Gephi-compatible CSV tables
to_graphml(edges, file = "net.graphml")       # GraphML (pure base R)
to_igraph(edges)                              # igraph graph object
to_matrix(edges)                              # adjacency matrix
```

## Counting methods

| Method | Description | Position-dependent |
|---|---|---|
| `"full"` | 1 per co-occurrence | No |
| `"fractional"` | 1 / (k_i × k_j) | No |
| `"paper"` | 1 / k_i | No |
| `"strength"` | 1 / k | No |
| `"harmonic"` | ∝ 1/position (Hagen 2008) | Yes |
| `"arithmetic"` | ∝ (n − pos + 1) | Yes |
| `"geometric"` | ∝ 2^(n − pos) (Liu & Fang 2023) | Yes |
| `"adaptive_geometric"` | Geometric adapted to author count | Yes |
| `"golden"` | ∝ φ^(n − pos) | Yes |
| `"first"` | First author only | Yes |
| `"last"` | Last author only | Yes |
| `"first_last"` | First and last upweighted | Yes |
| `"position_weighted"` | Custom weight vector | Yes |

## Attention weighting

Position-based weighting via the `attention` parameter, available on `author_network()`,
`keyword_network()`, `country_network()`, and `institution_network()`. When set, it runs
independently — `type` and `counting` are ignored. Inspired by attention-decay mechanisms
in Transition Network Analysis (Saqr et al.).

| `attention =` | Weight shape | Interpretation |
|---|---|---|
| `"lead"` | Quadratic drop from first | Lead/first author gets dominant credit |
| `"last"` | Quadratic rise to last | Last author (PI/supervisor) gets dominant credit |
| `"proximity"` | Pyramid peak at center | Middle-of-list authors weighted most |
| `"circular"` | High at both ends, low in middle | First and last equally prominent |

## Comparison

| Feature | bibnets | bibliometrix | biblionetwork |
|---|---|---|---|
| Counting methods | 13 | 2 | 4 |
| Attention weighting | 4 | — | — |
| Similarity measures | 6 | 3 | 6 |
| Position-dependent counting | Yes | No | No |
| Temporal networks | Yes | Partial | No |
| Backbone extraction | Yes | No | No |
| Readers | 9 | 7 | 0 |
| Hard dependencies | 3 | 30+ | 3 |
| S3 class + print/summary | Yes | Yes | No |

## References

- Batagelj, V., & Cerinsek, M. (2013). On bibliographic networks. *Scientometrics*, 96(3), 845–864.
- Hagen, N. T. (2008). Harmonic allocation of authorship credit. *Scientometrics*, 84(3), 785–793.
- Liu, W., & Fang, H. (2023). A geometric counting method adaptive to the author number. *Journal of Informetrics*, 17(2), 101397.
- Perianes-Rodriguez, A., Waltman, L., & van Eck, N. J. (2016). Constructing bibliometric networks. *Journal of Informetrics*, 10(4), 1089–1097.
- Serrano, M. Á., Boguñá, M., & Vespignani, A. (2009). Extracting the multiscale backbone of complex weighted networks. *PNAS*, 106(16), 6483–6488.
- van Eck, N. J., & Waltman, L. (2009). How to normalize cooccurrence data? *JASIST*, 60(8), 1635–1651.
- Saqr, M., López-Pernas, S., & Helske, S. (2024). Temporal network analysis of students' interactions. *Journal of Learning Analytics*.

## License

MIT
