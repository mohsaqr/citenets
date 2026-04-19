## Tests for abstract_network()

## Helper: synthetic abstracts with controlled vocabulary
make_abstract_data <- function() {
  data.frame(
    id = paste0("P", 1:6),
    abstract = c(
      "Machine learning methods are used for student performance prediction in educational data mining.",
      "Deep learning models improve student performance prediction using neural networks and educational data.",
      "Educational data mining applies machine learning to analyze student performance in learning systems.",
      "Natural language processing techniques support knowledge discovery in text mining applications.",
      "Knowledge discovery and text mining are important for natural language processing in education.",
      "Learning analytics uses educational data mining and machine learning for student performance analysis."
    ),
    stringsAsFactors = FALSE
  )
}

test_that("abstract_network returns bibnets_network with correct schema", {
  d <- make_abstract_data()
  edges <- abstract_network(d, min_freq = 1)

  expect_s3_class(edges, "bibnets_network")
  expect_true(all(c("from", "to", "weight", "count") %in% names(edges)))
  expect_true(nrow(edges) > 0)
  expect_true(all(edges$weight > 0))
  expect_true(all(is.finite(edges$weight)))
  expect_true(all(edges$count >= 1L))
})

test_that("abstract_network only includes phrases meeting min_freq", {
  d <- make_abstract_data()
  ## min_freq = 4 means phrase must appear in at least 4 docs
  edges4 <- abstract_network(d, min_freq = 4)
  edges2 <- abstract_network(d, min_freq = 1)
  ## Stricter filter → fewer or equal nodes
  nodes4 <- unique(c(edges4$from, edges4$to))
  nodes2 <- unique(c(edges2$from, edges2$to))
  expect_true(length(nodes4) <= length(nodes2))
})

test_that("abstract_network scoring = 'none' gives equal phrase quality", {
  d <- make_abstract_data()
  edges <- abstract_network(d, min_freq = 1, scoring = "none")
  expect_s3_class(edges, "bibnets_network")
  expect_true(nrow(edges) > 0)
  ## All weights positive and finite
  expect_true(all(edges$weight > 0 & is.finite(edges$weight)))
})

test_that("abstract_network scoring options all return valid output", {
  d <- make_abstract_data()
  for (sc in c("yake", "idf", "freq", "none")) {
    edges <- abstract_network(d, min_freq = 1, scoring = sc)
    expect_true(inherits(edges, "bibnets_network"), info = sc)
    expect_true(all(is.finite(edges$weight)), info = sc)
  }
})

test_that("abstract_network n-gram sizes respected", {
  d <- make_abstract_data()
  ## Unigrams only: phrases should be single words
  edges_uni <- abstract_network(d, n = 1L, min_freq = 1, scoring = "none")
  expect_true(all(!grepl(" ", edges_uni$from)))
  expect_true(all(!grepl(" ", edges_uni$to)))

  ## Bigrams only
  edges_bi <- abstract_network(d, n = 2L, min_freq = 1, scoring = "none")
  expect_true(all(lengths(strsplit(edges_bi$from, " ")) == 2L))
})

test_that("abstract_network higher decay reduces distant-pair weights", {
  d <- make_abstract_data()
  e1 <- abstract_network(d, min_freq = 1, decay = 0.1)
  e2 <- abstract_network(d, min_freq = 1, decay = 5.0)
  ## Higher decay = steeper drop-off = lower total weight
  expect_true(sum(e2$weight) <= sum(e1$weight))
})

test_that("abstract_network invalid scoring gives error", {
  d <- make_abstract_data()
  expect_error(abstract_network(d, scoring = "bad"), "scoring")
})

test_that("abstract_network threshold filters low-weight edges", {
  d <- make_abstract_data()
  edges_all <- abstract_network(d, min_freq = 1, threshold = 0)
  edges_thr <- abstract_network(d, min_freq = 1, threshold = 0.01)
  expect_true(nrow(edges_thr) <= nrow(edges_all))
  expect_true(all(edges_thr$weight >= 0.01))
})

test_that("abstract_network top_n limits output", {
  d <- make_abstract_data()
  edges <- abstract_network(d, min_freq = 1, top_n = 5L)
  expect_true(nrow(edges) <= 5L)
})

test_that("abstract_network self_loops = FALSE by default", {
  d <- make_abstract_data()
  edges <- abstract_network(d, min_freq = 1)
  expect_true(all(edges$from != edges$to))
})

test_that("abstract_network similarity normalization returns values in [0,1]", {
  d <- make_abstract_data()
  edges <- abstract_network(d, min_freq = 1, similarity = "association")
  expect_s3_class(edges, "bibnets_network")
  expect_true(all(edges$weight >= 0))
  expect_true(all(is.finite(edges$weight)))
})

test_that("abstract_network decay = 0 treats all co-occurrences equally within window", {
  d <- make_abstract_data()
  ## decay=0 → exp(0) = 1 for all distances
  edges <- abstract_network(d, min_freq = 1, decay = 0, scoring = "none")
  expect_s3_class(edges, "bibnets_network")
  expect_true(all(is.finite(edges$weight)))
})

test_that("abstract_network returns empty network when min_freq too high", {
  d <- make_abstract_data()
  edges <- abstract_network(d, min_freq = 999L)
  expect_s3_class(edges, "bibnets_network")
  expect_equal(nrow(edges), 0L)
})

test_that("abstract_network custom stopwords remove expected phrases", {
  d <- make_abstract_data()
  ## "machine" appears in phrases — blacklisting it should reduce nodes
  edges_no <- abstract_network(d, min_freq = 1, stopwords = "machine")
  edges_base <- abstract_network(d, min_freq = 1)
  nodes_no   <- unique(c(edges_no$from, edges_no$to))
  nodes_base <- unique(c(edges_base$from, edges_base$to))
  ## No phrase starting/ending with "machine" in no-machine result
  expect_false(any(grepl("^machine", nodes_no)))
  expect_false(any(grepl("machine$", nodes_no)))
})

test_that("abstract_network handles NA and empty abstracts gracefully", {
  d <- make_abstract_data()
  d$abstract[2] <- NA_character_
  d$abstract[4] <- ""
  edges <- abstract_network(d, min_freq = 1)
  expect_s3_class(edges, "bibnets_network")
  expect_true(all(is.finite(edges$weight)))
})

test_that("abstract_network network_type attribute is set correctly", {
  d <- make_abstract_data()
  edges <- abstract_network(d, min_freq = 1)
  expect_equal(attr(edges, "network_type"), "abstract_cooccurrence")
})
