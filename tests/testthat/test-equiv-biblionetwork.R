## Cross-validation: bibnets vs biblionetwork (Goutsmedt et al.)
## All reference values are computed live from biblionetwork — no hardcoded weights.

## ---------------------------------------------------------------------------
## Helpers
## ---------------------------------------------------------------------------

## Expand a bibnets list-column data frame to long-format data.table.
to_long_dt <- function(d, id_col = "id", entity_col) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  rows <- lapply(seq_len(nrow(d)), function(i) {
    ents <- d[[entity_col]][[i]]
    if (length(ents) == 0L) return(NULL)
    data.frame(
      article = d[[id_col]][i],
      entity  = toupper(trimws(ents)),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df <- df[!is.na(df$entity) & nchar(df$entity) > 0, , drop = FALSE]
  data.table::as.data.table(df)
}

## Align two edge lists by canonical undirected pair key.
## Returns a data frame with w1 (bibnets) and w2 (biblionetwork).
align_edges <- function(e1, e2) {
  key1 <- paste(pmin(e1$from, e1$to), pmax(e1$from, e1$to), sep = "||")
  key2 <- paste(pmin(e2$from, e2$to), pmax(e2$from, e2$to), sep = "||")
  shared <- intersect(key1, key2)
  if (length(shared) == 0L)
    return(data.frame(key = character(), w1 = numeric(), w2 = numeric()))
  data.frame(
    key = shared,
    w1  = e1$weight[match(shared, key1)],
    w2  = e2$weight[match(shared, key2)],
    stringsAsFactors = FALSE
  )
}

## ---------------------------------------------------------------------------
## Co-authorship
## ---------------------------------------------------------------------------

test_that("co-authorship full counting matches biblionetwork full_counting", {
  skip_if_not_installed("biblionetwork")
  skip_if_not_installed("data.table")

  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  dt <- to_long_dt(d, "id", "authors")
  bn <- as.data.frame(
    biblionetwork::coauth_network(dt, authors = "entity", articles = "article",
                                  method = "full_counting")
  )

  bibnets_edges <- author_network(d, "collaboration", counting = "full")

  aligned <- align_edges(bibnets_edges, bn)
  expect_gt(nrow(aligned), 0L)
  expect_equal(aligned$w1, aligned$w2, tolerance = 1e-10,
               label = "bibnets full", expected.label = "biblionetwork full_counting")
})


test_that("co-authorship fractional matches biblionetwork fractional_counting", {
  skip_if_not_installed("biblionetwork")
  skip_if_not_installed("data.table")

  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  dt <- to_long_dt(d, "id", "authors")
  bn <- as.data.frame(
    biblionetwork::coauth_network(dt, authors = "entity", articles = "article",
                                  method = "fractional_counting")
  )

  bibnets_edges <- author_network(d, "collaboration", counting = "fractional")

  aligned <- align_edges(bibnets_edges, bn)
  expect_gt(nrow(aligned), 0L)
  expect_equal(aligned$w1, aligned$w2, tolerance = 1e-10,
               label = "bibnets fractional", expected.label = "biblionetwork fractional_counting")
})


test_that("co-authorship paper counting matches biblionetwork fractional_counting_refined", {
  skip_if_not_installed("biblionetwork")
  skip_if_not_installed("data.table")

  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  dt <- to_long_dt(d, "id", "authors")
  bn <- as.data.frame(
    biblionetwork::coauth_network(dt, authors = "entity", articles = "article",
                                  method = "fractional_counting_refined")
  )

  bibnets_edges <- author_network(d, "collaboration", counting = "paper")

  aligned <- align_edges(bibnets_edges, bn)
  expect_gt(nrow(aligned), 0L)
  expect_equal(aligned$w1, aligned$w2, tolerance = 1e-10,
               label = "bibnets paper", expected.label = "biblionetwork fractional_counting_refined")
})


test_that("co-authorship full + cosine matches biblionetwork cosine_normalized", {
  skip_if_not_installed("biblionetwork")
  skip_if_not_installed("data.table")

  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  dt <- to_long_dt(d, "id", "authors")
  bn <- as.data.frame(
    biblionetwork::coauth_network(dt, authors = "entity", articles = "article",
                                  method = "full_counting", cosine_normalized = TRUE)
  )

  bibnets_edges <- author_network(d, "collaboration", counting = "full", similarity = "cosine")

  aligned <- align_edges(bibnets_edges, bn)
  expect_gt(nrow(aligned), 0L)
  expect_equal(aligned$w1, aligned$w2, tolerance = 1e-6,
               label = "bibnets cosine", expected.label = "biblionetwork cosine_normalized")
})


## ---------------------------------------------------------------------------
## Bibliographic coupling
## ---------------------------------------------------------------------------

test_that("coupling cosine matches biblionetwork biblio_coupling", {
  skip_if_not_installed("biblionetwork")
  skip_if_not_installed("data.table")

  d <- data.frame(id = paste0("W", 1:5), stringsAsFactors = FALSE)
  d$references <- list(
    c("R1", "R2", "R3"), c("R1", "R2", "R4"), c("R2", "R3", "R5"),
    c("R1", "R3", "R4"), c("R2", "R5", "R6")
  )

  dt <- to_long_dt(d, "id", "references")
  bn <- as.data.frame(
    biblionetwork::biblio_coupling(dt, source = "article", ref = "entity",
                                   normalized_weight_only = TRUE)
  )

  bibnets_edges <- document_network(d, "coupling", similarity = "cosine")

  aligned <- align_edges(bibnets_edges, bn)
  expect_gt(nrow(aligned), 0L)
  expect_equal(aligned$w1, aligned$w2, tolerance = 1e-6,
               label = "bibnets coupling cosine", expected.label = "biblionetwork biblio_coupling")
})


## ---------------------------------------------------------------------------
## Co-citation
## ---------------------------------------------------------------------------

test_that("co-citation cosine matches biblionetwork biblio_cocitation", {
  skip_if_not_installed("biblionetwork")
  skip_if_not_installed("data.table")

  d <- data.frame(id = paste0("W", 1:5), stringsAsFactors = FALSE)
  d$references <- list(
    c("R1", "R2", "R3"), c("R1", "R2", "R4"), c("R2", "R3", "R5"),
    c("R1", "R3", "R4"), c("R2", "R5", "R6")
  )

  dt <- to_long_dt(d, "id", "references")
  bn <- as.data.frame(
    biblionetwork::biblio_cocitation(dt, source = "article", ref = "entity",
                                     normalized_weight_only = TRUE)
  )

  bibnets_edges <- reference_network(d, similarity = "cosine", threshold = 0)

  aligned <- align_edges(bibnets_edges, bn)
  expect_gt(nrow(aligned), 0L)
  expect_equal(aligned$w1, aligned$w2, tolerance = 1e-6,
               label = "bibnets co-citation cosine", expected.label = "biblionetwork biblio_cocitation")
})
