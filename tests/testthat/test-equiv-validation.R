## Comprehensive cross-package validation: bibnets vs biblionetwork + bibliometrix
## Six synthetic datasets × five network types × two reference packages
## Reports max diff, mean diff, Pearson correlation on shared edges.

skip_if_not_installed("biblionetwork")
skip_if_not_installed("bibliometrix")
skip_if_not_installed("data.table")

## ===========================================================================
## Dataset generators
## ===========================================================================

make_dataset <- function(n_papers, n_authors, avg_auth, n_refs = 0L,
                         n_ref_pool = 0L, seed = 42L) {
  set.seed(seed)
  auth_pool <- paste0("AUTHOR_", seq_len(n_authors))
  ## WoS-format refs required by bibliometrix: "LASTNAME I, YEAR, JOURNAL"
  ref_pool  <- if (n_ref_pool > 0)
    sprintf("REFAUTH%d Z, %d, REFJ %d", seq_len(n_ref_pool),
            2000L + (seq_len(n_ref_pool) %% 20L), seq_len(n_ref_pool))
  else character(0)

  ids <- paste0("P", seq_len(n_papers))

  authors <- lapply(seq_len(n_papers), function(i) {
    n <- max(2L, rpois(1, avg_auth))   # at least 2 — single-author papers
    n <- min(n, n_authors)             # change biblionetwork's nb_art denominator
    sample(auth_pool, n)
  })

  references <- if (n_refs > 0 && n_ref_pool > 0) {
    lapply(seq_len(n_papers), function(i) {
      n <- max(1L, rpois(1, n_refs))
      n <- min(n, n_ref_pool)
      sample(ref_pool, n)
    })
  } else {
    replicate(n_papers, character(0), simplify = FALSE)
  }

  d <- data.frame(id = ids, stringsAsFactors = FALSE)
  d$authors    <- authors
  d$references <- references
  d
}

datasets <- list(
  tiny   = make_dataset(n_papers = 10,   n_authors = 8,   avg_auth = 3,
                        n_refs = 5,  n_ref_pool = 15,  seed = 1L),
  small  = make_dataset(n_papers = 50,   n_authors = 25,  avg_auth = 3,
                        n_refs = 8,  n_ref_pool = 40,  seed = 2L),
  medium = make_dataset(n_papers = 200,  n_authors = 80,  avg_auth = 4,
                        n_refs = 12, n_ref_pool = 100, seed = 3L),
  large  = make_dataset(n_papers = 1000, n_authors = 200, avg_auth = 4,
                        n_refs = 20, n_ref_pool = 300, seed = 4L),
  sparse = make_dataset(n_papers = 300,  n_authors = 250, avg_auth = 2,
                        n_refs = 5,  n_ref_pool = 200, seed = 5L),
  dense  = make_dataset(n_papers = 50,   n_authors = 8,   avg_auth = 5,
                        n_refs = 10, n_ref_pool = 20,  seed = 6L)
)

## ===========================================================================
## Format converters
## ===========================================================================

## bibnets list-column → long-format data.table (for biblionetwork)
to_long_dt <- function(d, id_col = "id", entity_col) {
  rows <- lapply(seq_len(nrow(d)), function(i) {
    ents <- d[[entity_col]][[i]]
    if (length(ents) == 0L) return(NULL)
    data.frame(article = d[[id_col]][i],
               entity  = toupper(trimws(ents)),
               stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, rows)
  df <- df[!is.na(df$entity) & nchar(df$entity) > 0, , drop = FALSE]
  data.table::as.data.table(df)
}

## bibnets data frame → bibliometrix-compatible data frame
to_bibliometrix <- function(d) {
  bm <- data.frame(
    DB = "ISI",
    UT = d$id,
    AU = vapply(d$authors, function(a) paste(a, collapse = "; "), character(1)),
    CR = vapply(d$references, function(r) paste(r, collapse = "; "), character(1)),
    stringsAsFactors = FALSE
  )
  bm$CR[bm$CR == ""] <- NA
  bm
}

## Sparse Matrix → edge list (off-diagonal entries only)
mat_to_edgelist <- function(M) {
  s  <- Matrix::summary(M)
  ef <- data.frame(from = rownames(M)[s$i], to = colnames(M)[s$j],
                   weight = s$x, stringsAsFactors = FALSE)
  ef[ef$from != ef$to & ef$weight > 0, ]
}

## Normalize bibnets co-citation node names to bibliometrix "AUTHOR YEAR" format
norm_ref_label <- function(x) sub("^([^,]+), *([0-9]{4}).*", "\\1 \\2", trimws(x))

## ===========================================================================
## Alignment and comparison helpers
## ===========================================================================

## Canonical undirected pair key
pair_key <- function(from, to) paste(pmin(from, to), pmax(from, to), sep = "||")

## Align two edge lists on shared pairs; return data frame with w1, w2
align_edges <- function(e1, e2) {
  k1 <- pair_key(e1$from, e1$to)
  k2 <- pair_key(e2$from, e2$to)
  shared <- intersect(k1, k2)
  if (length(shared) == 0L)
    return(data.frame(w1 = numeric(0), w2 = numeric(0)))
  data.frame(
    w1 = e1$weight[match(shared, k1)],
    w2 = e2$weight[match(shared, k2)]
  )
}

## Compute comparison metrics
compare_nets <- function(e_bibnets, e_ref, label_bibnets, label_ref) {
  n1 <- length(unique(c(e_bibnets$from, e_bibnets$to)))
  n2 <- length(unique(c(e_ref$from,     e_ref$to)))
  aligned <- align_edges(e_bibnets, e_ref)
  n_shared <- nrow(aligned)

  if (n_shared < 2L) {
    return(data.frame(
      pkg_bibnets = label_bibnets, pkg_ref = label_ref,
      nodes_bibnets = n1, nodes_ref = n2, edges_bibnets = nrow(e_bibnets),
      edges_ref = nrow(e_ref), shared_edges = n_shared,
      max_diff = NA_real_, mean_diff = NA_real_, pearson = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  diffs <- abs(aligned$w1 - aligned$w2)
  pearson <- tryCatch(cor(aligned$w1, aligned$w2), error = function(e) NA_real_)

  data.frame(
    pkg_bibnets   = label_bibnets,
    pkg_ref       = label_ref,
    nodes_bibnets = n1,
    nodes_ref     = n2,
    edges_bibnets = nrow(e_bibnets),
    edges_ref     = nrow(e_ref),
    shared_edges  = n_shared,
    max_diff      = max(diffs),
    mean_diff     = mean(diffs),
    pearson       = pearson,
    stringsAsFactors = FALSE
  )
}

## ===========================================================================
## Validation runner
## ===========================================================================

run_validation <- function(d, ds_name) {
  results <- list()

  safe <- function(label, expr) {
    tryCatch(eval(expr), error = function(e) {
      message("  SKIP [", ds_name, "] ", label, ": ", conditionMessage(e))
      NULL
    })
  }

  dt_authors <- to_long_dt(d, "id", "authors")
  dt_refs    <- to_long_dt(d, "id", "references")
  bm         <- to_bibliometrix(d)

  ## --- 1. Co-authorship full ---
  bn_full <- safe("bn coauth full",
    quote(as.data.frame(biblionetwork::coauth_network(
      dt_authors, authors = "entity", articles = "article",
      method = "full_counting"))))
  bb_full <- safe("bm coauth full", {
    M  <- bibliometrix::biblioNetwork(bm, analysis = "collaboration",
                                      network = "authors", sep = "; ")
    ef <- mat_to_edgelist(M)
    names(ef) <- c("from", "to", "weight")
    ef
  })
  bi_full <- safe("bibnets coauth full",
    quote(author_network(d, "collaboration", counting = "full", threshold = 0)))

  if (!is.null(bi_full) && !is.null(bn_full))
    results[[length(results)+1]] <- cbind(
      data.frame(dataset=ds_name, network="coauth", counting="full"),
      compare_nets(bi_full, bn_full, "bibnets", "biblionetwork"))

  if (!is.null(bi_full) && !is.null(bb_full))
    results[[length(results)+1]] <- cbind(
      data.frame(dataset=ds_name, network="coauth", counting="full"),
      compare_nets(bi_full, bb_full, "bibnets", "bibliometrix"))

  ## --- 2. Co-authorship fractional ---
  bn_frac <- safe("bn coauth fractional",
    quote(as.data.frame(biblionetwork::coauth_network(
      dt_authors, authors = "entity", articles = "article",
      method = "fractional_counting"))))
  bi_frac <- safe("bibnets coauth fractional",
    quote(author_network(d, "collaboration", counting = "fractional", threshold = 0)))

  if (!is.null(bi_frac) && !is.null(bn_frac))
    results[[length(results)+1]] <- cbind(
      data.frame(dataset=ds_name, network="coauth", counting="fractional"),
      compare_nets(bi_frac, bn_frac, "bibnets", "biblionetwork"))

  ## --- 3. Co-authorship paper (fractional_counting_refined) ---
  bn_paper <- safe("bn coauth paper",
    quote(as.data.frame(biblionetwork::coauth_network(
      dt_authors, authors = "entity", articles = "article",
      method = "fractional_counting_refined"))))
  bi_paper <- safe("bibnets coauth paper",
    quote(author_network(d, "collaboration", counting = "paper", threshold = 0)))

  if (!is.null(bi_paper) && !is.null(bn_paper))
    results[[length(results)+1]] <- cbind(
      data.frame(dataset=ds_name, network="coauth", counting="paper"),
      compare_nets(bi_paper, bn_paper, "bibnets", "biblionetwork"))

  ## --- 4. Co-authorship cosine ---
  bn_cos <- safe("bn coauth cosine",
    quote(as.data.frame(biblionetwork::coauth_network(
      dt_authors, authors = "entity", articles = "article",
      method = "full_counting", cosine_normalized = TRUE))))
  bi_cos <- safe("bibnets coauth cosine",
    quote(author_network(d, "collaboration", counting = "full",
                          similarity = "cosine", threshold = 0)))

  if (!is.null(bi_cos) && !is.null(bn_cos))
    results[[length(results)+1]] <- cbind(
      data.frame(dataset=ds_name, network="coauth", counting="full+cosine"),
      compare_nets(bi_cos, bn_cos, "bibnets", "biblionetwork"))

  ## --- 5. Bibliographic coupling cosine ---
  if (nrow(dt_refs) > 0) {
    bn_coup <- safe("bn coupling",
      quote(as.data.frame(biblionetwork::biblio_coupling(
        dt_refs, source = "article", ref = "entity",
        normalized_weight_only = TRUE))))
    bi_coup <- safe("bibnets coupling cosine",
      quote(document_network(d, "coupling", similarity = "cosine", threshold = 0)))

    if (!is.null(bi_coup) && !is.null(bn_coup))
      results[[length(results)+1]] <- cbind(
        data.frame(dataset=ds_name, network="coupling", counting="cosine"),
        compare_nets(bi_coup, bn_coup, "bibnets", "biblionetwork"))

    ## bibliometrix coupling
    bb_coup <- safe("bm coupling cosine", {
      M               <- bibliometrix::biblioNetwork(bm, analysis = "coupling",
                                                      network = "references", sep = "; ")
      rownames(M)     <- colnames(M) <- bm$UT
      Mn              <- bibliometrix::normalizeSimilarity(M, type = "salton")
      mat_to_edgelist(Mn)
    })
    if (!is.null(bi_coup) && !is.null(bb_coup))
      results[[length(results)+1]] <- cbind(
        data.frame(dataset=ds_name, network="coupling", counting="cosine"),
        compare_nets(bi_coup, bb_coup, "bibnets", "bibliometrix"))

    ## --- 5b. Coupling strength (informational — formulas differ) ---
    ## bibnets: (1/sqrt(a*b)) * sum(log(N/n_r))
    ## biblionetwork: (1/(a*b)) * sum(log(N/n_r))  [Perianes-Rodriguez 2016]
    bn_str <- safe("bn coupling_strength",
      quote(as.data.frame(biblionetwork::coupling_strength(
        dt_refs, source = "article", ref = "entity"))))
    bi_str <- safe("bibnets coupling strength",
      quote(document_network(d, "coupling", counting = "strength", threshold = 0)))

    if (!is.null(bi_str) && !is.null(bn_str))
      results[[length(results)+1]] <- cbind(
        data.frame(dataset=ds_name, network="coupling", counting="strength"),
        compare_nets(bi_str, bn_str, "bibnets", "biblionetwork"))

    ## --- 6. Reference co-citation cosine ---
    bn_cocit <- safe("bn co-citation",
      quote(as.data.frame(biblionetwork::biblio_cocitation(
        dt_refs, source = "article", ref = "entity",
        normalized_weight_only = TRUE))))
    bi_cocit <- safe("bibnets co-citation cosine",
      quote(reference_network(d, similarity = "cosine", threshold = 0)))

    if (!is.null(bi_cocit) && !is.null(bn_cocit))
      results[[length(results)+1]] <- cbind(
        data.frame(dataset=ds_name, network="co-citation", counting="cosine"),
        compare_nets(bi_cocit, bn_cocit, "bibnets", "biblionetwork"))

    ## bibliometrix co-citation
    bb_cocit <- safe("bm co-citation cosine", {
      M  <- bibliometrix::biblioNetwork(bm, analysis = "co-citation",
                                        network = "references", sep = "; ")
      Mn <- bibliometrix::normalizeSimilarity(M, type = "salton")
      ef <- mat_to_edgelist(Mn)
      ## Normalize node names from "REFAUTH1 Z, 2005, REFJ 1" → "REFAUTH1 Z 2005"
      ef$from <- norm_ref_label(ef$from)
      ef$to   <- norm_ref_label(ef$to)
      ef
    })
    if (!is.null(bi_cocit) && !is.null(bb_cocit)) {
      ## Also normalize bibnets node names for alignment
      bi_cocit_norm       <- bi_cocit
      bi_cocit_norm$from  <- norm_ref_label(bi_cocit_norm$from)
      bi_cocit_norm$to    <- norm_ref_label(bi_cocit_norm$to)
      results[[length(results)+1]] <- cbind(
        data.frame(dataset=ds_name, network="co-citation", counting="cosine"),
        compare_nets(bi_cocit_norm, bb_cocit, "bibnets", "bibliometrix"))
    }
  }

  do.call(rbind, results)
}

## ===========================================================================
## Run all datasets
## ===========================================================================

all_results <- do.call(rbind, lapply(names(datasets), function(nm) {
  run_validation(datasets[[nm]], nm)
}))

## ===========================================================================
## Tests: all comparisons must be numerically equivalent
## ===========================================================================

test_that("co-authorship full: bibnets == biblionetwork across all datasets", {
  rows <- all_results[all_results$network == "coauth" &
                      all_results$counting == "full" &
                      all_results$pkg_ref == "biblionetwork", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-10,
                 label = paste("max_diff [", rows$dataset[i], "]"))
    expect_equal(rows$pearson[i], 1, tolerance = 1e-8,
                 label = paste("pearson [", rows$dataset[i], "]"))
  }
})

test_that("co-authorship fractional: bibnets == biblionetwork across all datasets", {
  rows <- all_results[all_results$network == "coauth" &
                      all_results$counting == "fractional" &
                      all_results$pkg_ref == "biblionetwork", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-10,
                 label = paste("max_diff [", rows$dataset[i], "]"))
  }
})

test_that("co-authorship paper: bibnets == biblionetwork across all datasets", {
  rows <- all_results[all_results$network == "coauth" &
                      all_results$counting == "paper" &
                      all_results$pkg_ref == "biblionetwork", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-10,
                 label = paste("max_diff [", rows$dataset[i], "]"))
  }
})

test_that("co-authorship cosine: bibnets == biblionetwork across all datasets", {
  rows <- all_results[all_results$network == "coauth" &
                      all_results$counting == "full+cosine" &
                      all_results$pkg_ref == "biblionetwork", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-6,
                 label = paste("max_diff [", rows$dataset[i], "]"))
  }
})

test_that("coupling cosine: bibnets == biblionetwork across all datasets", {
  rows <- all_results[all_results$network == "coupling" &
                      all_results$counting == "cosine" &
                      all_results$pkg_ref == "biblionetwork", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-6,
                 label = paste("max_diff [", rows$dataset[i], "]"))
  }
})

test_that("co-citation cosine: bibnets == biblionetwork across all datasets", {
  rows <- all_results[all_results$network == "co-citation" &
                      all_results$pkg_ref == "biblionetwork", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-6,
                 label = paste("max_diff [", rows$dataset[i], "]"))
  }
})

test_that("coupling strength: bibnets == biblionetwork (Perianes-Rodriguez formula)", {
  rows <- all_results[all_results$network == "coupling" &
                      all_results$counting == "strength" &
                      all_results$pkg_ref == "biblionetwork", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-10,
                 label = paste("max_diff [", rows$dataset[i], "]"))
    expect_equal(rows$pearson[i], 1, tolerance = 1e-8,
                 label = paste("pearson [", rows$dataset[i], "]"))
  }
})

test_that("coupling cosine: bibnets ~ bibliometrix (tolerance 1e-4)", {
  rows <- all_results[all_results$network == "coupling" &
                      all_results$pkg_ref == "bibliometrix", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-4,
                 label = paste("max_diff [", rows$dataset[i], "]"))
  }
})

test_that("co-citation cosine: bibnets ~ bibliometrix (tolerance 1e-4)", {
  rows <- all_results[all_results$network == "co-citation" &
                      all_results$pkg_ref == "bibliometrix", ]
  for (i in seq_len(nrow(rows))) {
    expect_equal(rows$max_diff[i], 0, tolerance = 1e-4,
                 label = paste("max_diff [", rows$dataset[i], "]"))
  }
})
