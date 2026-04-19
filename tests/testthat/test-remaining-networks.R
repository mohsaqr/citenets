## Tests for source_network(), country_network(), institution_network(), conetwork()
## All should return bibnets_network S3 objects.

## Extended test data with countries and affiliations
make_extended_data <- function() {
  d <- make_test_data()
  d$countries <- list(
    c("USA", "UK"),
    c("USA", "DE"),
    c("UK", "DE")
  )
  d$affiliations <- list(
    c("MIT", "Oxford"),
    c("MIT", "TU Berlin"),
    c("Oxford", "TU Berlin")
  )
  d
}

## ── source_network ──────────────────────────────────────────────────────────

test_that("source_network coupling returns bibnets_network", {
  d <- make_test_data()
  edges <- source_network(d, type = "coupling")
  expect_s3_class(edges, "bibnets_network")
  expect_true(all(c("from", "to", "weight") %in% names(edges)))
  expect_equal(attr(edges, "network_type"), "source_coupling")
})

test_that("source_network coupling produces journal nodes", {
  d <- make_test_data()
  edges <- source_network(d, type = "coupling")
  ## J1 and J2 both cite shared refs — should be linked
  all_nodes <- unique(c(edges$from, edges$to))
  expect_true(all(all_nodes %in% c("J1", "J2")))
})

test_that("source_network equivalence returns bibnets_network", {
  d <- make_test_data()
  edges <- source_network(d, type = "equivalence")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "source_equivalence")
})

test_that("source_network co_citation stops without cited_journals", {
  d <- make_test_data()
  expect_error(source_network(d, type = "co_citation"))
})

test_that("source_network co_citation works with cited_journals column", {
  d <- make_test_data()
  d$cited_journals <- list(
    c("J1", "J2"),
    c("J1", "J3"),
    c("J2", "J3")
  )
  edges <- source_network(d, type = "co_citation")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "source_co_citation")
})

test_that("source_network coupling requires journal column", {
  d <- make_test_data()
  d$journal <- NULL
  expect_error(source_network(d, type = "coupling"))
})

## ── country_network ─────────────────────────────────────────────────────────

test_that("country_network collaboration returns bibnets_network", {
  d <- make_extended_data()
  edges <- country_network(d, type = "collaboration")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "country_collaboration")
})

test_that("country_network collaboration has correct country nodes", {
  d <- make_extended_data()
  edges <- country_network(d, type = "collaboration")
  all_nodes <- unique(c(edges$from, edges$to))
  expect_true(all(all_nodes %in% c("USA", "UK", "DE")))
})

test_that("country_network collaboration edge counts are correct", {
  d <- make_extended_data()
  edges <- country_network(d, type = "collaboration")

  get_w <- function(a, b) {
    row <- edges[(edges$from == a & edges$to == b) |
                   (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }
  ## USA-UK: W1; USA-DE: W2; UK-DE: W3
  expect_equal(get_w("USA", "UK"), 1)
  expect_equal(get_w("USA", "DE"), 1)
  expect_equal(get_w("UK",  "DE"), 1)
})

test_that("country_network coupling returns bibnets_network", {
  d <- make_extended_data()
  edges <- country_network(d, type = "coupling")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "country_coupling")
})

test_that("country_network equivalence returns bibnets_network", {
  d <- make_extended_data()
  edges <- country_network(d, type = "equivalence")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "country_equivalence")
})

## ── institution_network ─────────────────────────────────────────────────────

test_that("institution_network collaboration returns bibnets_network", {
  d <- make_extended_data()
  edges <- institution_network(d, type = "collaboration")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "institution_collaboration")
})

test_that("institution_network collaboration has correct nodes", {
  d <- make_extended_data()
  edges <- institution_network(d, type = "collaboration")
  all_nodes <- unique(c(edges$from, edges$to))
  ## build_bipartite uppercases all entity names
  expect_true(all(all_nodes %in% c("MIT", "OXFORD", "TU BERLIN")))
})

test_that("institution_network coupling returns bibnets_network", {
  d <- make_extended_data()
  edges <- institution_network(d, type = "coupling")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "institution_coupling")
})

test_that("institution_network equivalence returns bibnets_network", {
  d <- make_extended_data()
  edges <- institution_network(d, type = "equivalence")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "institution_equivalence")
})

## ── conetwork ───────────────────────────────────────────────────────────────

test_that("conetwork co-occurrence returns bibnets_network", {
  d <- make_test_data()
  edges <- conetwork(d, "keywords")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "keywords_co_occurrence")
})

test_that("conetwork co-occurrence has correct keyword nodes", {
  d <- make_test_data()
  edges <- conetwork(d, "keywords")
  all_nodes <- unique(c(edges$from, edges$to))
  ## build_bipartite uppercases all entity names
  expect_true(all(all_nodes %in% c("ML", "DL", "NLP", "CV")))
})

test_that("conetwork by= returns bibnets_network with by_ label", {
  d <- make_test_data()
  edges <- conetwork(d, "authors", by = "keywords")
  expect_s3_class(edges, "bibnets_network")
  expect_equal(attr(edges, "network_type"), "authors_by_keywords")
})

test_that("conetwork by= links entities through shared values", {
  d <- make_test_data()
  ## Authors sharing a keyword are linked
  edges <- conetwork(d, "authors", by = "keywords")
  ## Alice and Carol both use nlp (via W2)
  all_nodes <- unique(c(edges$from, edges$to))
  ## At least some authors should be linked
  expect_true(nrow(edges) > 0)
})

test_that("conetwork splits string columns on sep", {
  d <- data.frame(
    id   = c("W1", "W2", "W3"),
    tags = c("ml; dl", "ml; nlp", "dl; nlp"),
    stringsAsFactors = FALSE
  )
  edges <- conetwork(d, "tags")
  expect_true(is.data.frame(edges))
  expect_true(nrow(edges) > 0)
  all_nodes <- unique(c(edges$from, edges$to))
  ## build_bipartite uppercases entity names
  expect_true(any(grepl("ML|DL|NLP", all_nodes)))
})

test_that("conetwork counting attribute is preserved", {
  d <- make_test_data()
  edges <- conetwork(d, "keywords", counting = "fractional")
  expect_equal(attr(edges, "counting"), "fractional")
})

test_that("conetwork similarity attribute is preserved", {
  d <- make_test_data()
  edges <- conetwork(d, "keywords", similarity = "association")
  expect_equal(attr(edges, "similarity"), "association")
})
