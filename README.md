# citenets

> **Early development** — API may change. Feedback welcome.

R package for constructing citation, co-citation, bibliographic coupling, co-authorship, and keyword co-occurrence networks from scholarly data.

## What it does

- **13 counting methods** including harmonic, geometric, golden ratio, and adaptive geometric (9 more than any other R package)
- **6 similarity measures**: association strength, cosine, Jaccard, inclusion, equivalence
- **8 network functions**: author, document, reference, keyword, institution, country, source, plus generic `conetwork()`
- **6 readers**: Scopus, Web of Science, OpenAlex, BibTeX, RIS, Lens.org
- **3 dependencies**: Matrix, stats, utils
- Faster than both bibliometrix and biblionetwork on co-authorship networks
- Numerically validated against bibliometrix and biblionetwork

## Install

```r
# install.packages("remotes")
remotes::install_github("mohsaqr/citenets")
```

## Quick start

```r
library(citenets)

# Read data
data <- read_scopus("export.csv")

# Build networks
author_network(data, "collaboration", counting = "harmonic")
reference_network(data, similarity = "association")
document_network(data, "coupling", similarity = "cosine")
keyword_network(data)

# Generic: works with any data frame
conetwork(raw_df, "Authors", sep = ";")
conetwork(raw_df, "Authors", by = "Keywords", sep = ";")

# Convert
edges <- author_network(data, "collaboration")
to_igraph(edges)
to_matrix(edges)
```

## Tutorial

See `tutorials/citenets-tutorial.qmd` for a comprehensive walkthrough with equivalence tests.

## License

MIT
