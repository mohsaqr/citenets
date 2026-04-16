## Tests for S3 print/summary methods and as_citenets_network()

test_that("citenets_network has correct class", {
  d <- make_test_data()
  edges <- keyword_network(d)
  expect_s3_class(edges, "citenets_network")
  expect_s3_class(edges, "data.frame")
})

test_that("all network functions return citenets_network", {
  d <- make_test_data()
  dext <- d
  dext$countries    <- list(c("USA","UK"), c("USA","DE"), c("UK","DE"))
  dext$affiliations <- list(c("MIT","Oxford"), c("MIT","TU Berlin"), c("Oxford","TU Berlin"))

  fns <- list(
    keyword_network(d),
    author_network(d, "collaboration"),
    reference_network(d, threshold = 0),
    document_network(d, "coupling"),
    source_network(d, "coupling"),
    country_network(dext, "collaboration"),
    institution_network(dext, "collaboration"),
    conetwork(d, "keywords")
  )

  for (e in fns) {
    expect_true(inherits(e, "citenets_network"))
  }
})

test_that("network_type attribute is set correctly", {
  d <- make_test_data()
  expect_equal(attr(keyword_network(d), "network_type"), "keyword_co_occurrence")
  expect_equal(attr(author_network(d, "collaboration"), "network_type"), "author_collaboration")
  expect_equal(attr(author_network(d, "coupling"), "network_type"), "author_coupling")
  expect_equal(attr(reference_network(d, threshold=0), "network_type"), "reference_co_citation")
  expect_equal(attr(document_network(d, "coupling"), "network_type"), "document_coupling")
  expect_equal(attr(document_network(d, "citation"), "network_type"), "document_citation")
})

test_that("print.citenets_network outputs correctly", {
  d <- make_test_data()
  edges <- keyword_network(d)
  out <- capture.output(print(edges))
  ## First line should mention nodes and edges
  expect_true(grepl("citenets network", out[1]))
  expect_true(grepl("nodes", out[1]))
  expect_true(grepl("edges", out[1]))
  ## Should mention the type
  expect_true(any(grepl("keyword_co_occurrence", out)))
})

test_that("print.citenets_network n= argument limits rows shown", {
  d <- make_test_data()
  edges <- keyword_network(d)
  ## Show only 2 rows
  out <- capture.output(print(edges, n = 2))
  ## Should mention "more edges" if there are more than 2
  if (nrow(edges) > 2) {
    expect_true(any(grepl("more edges", out)))
  }
})

test_that("summary.citenets_network outputs correctly", {
  d <- make_test_data()
  edges <- keyword_network(d)
  out <- capture.output(summary(edges))
  expect_true(any(grepl("citenets network", out)))
  expect_true(any(grepl("Nodes", out)))
  expect_true(any(grepl("Edges", out)))
  expect_true(any(grepl("Density", out)))
  expect_true(any(grepl("Top nodes", out)))
})

test_that("summary.citenets_network returns object invisibly", {
  d <- make_test_data()
  edges <- keyword_network(d)
  ret <- capture.output(x <- summary(edges))
  expect_identical(x, edges)
})

test_that("print.citenets_network returns object invisibly", {
  d <- make_test_data()
  edges <- keyword_network(d)
  ret <- capture.output(x <- print(edges))
  expect_identical(x, edges)
})

test_that("as_citenets_network stamps correct attributes", {
  df <- data.frame(from = "A", to = "B", weight = 1, stringsAsFactors = FALSE)
  net <- citenets:::as_citenets_network(df, "test_type", "full", "cosine")
  expect_s3_class(net, "citenets_network")
  expect_equal(attr(net, "network_type"), "test_type")
  expect_equal(attr(net, "counting"),     "full")
  expect_equal(attr(net, "similarity"),   "cosine")
})
