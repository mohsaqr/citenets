test_that("author_network collaboration with full counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration")

  expect_true(is.data.frame(edges))
  expect_true(all(c("from", "to", "weight", "count", "shared") %in% names(edges)))

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("Alice", "Bob"), 1)
  expect_equal(get_w("Alice", "Carol"), 1)
  expect_equal(get_w("Bob", "Carol"), 1)
  expect_equal(get_w("Alice", "Dan"), 0)
})

test_that("author_network collaboration with harmonic counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", count = "harmonic")

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  ## W1: Alice (2/3), Bob (1/3) -> co-auth = 2/3 * 1/3 = 2/9
  expect_equal(get_w("Alice", "Bob"), 2/9, tolerance = tol)

  ## W3: Bob (1/H3), Carol ((1/2)/H3), Dan ((1/3)/H3)
  h3 <- 1 + 1/2 + 1/3
  ## Bob-Carol from W3: (1/h3) * ((1/2)/h3)
  expect_equal(get_w("Bob", "Carol"), (1/h3) * ((1/2)/h3), tolerance = tol)
})

test_that("author_network collaboration with geometric counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", count = "geometric")

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  ## W2: Alice (pos 1), Carol (pos 2), n=2
  ## geometric: (1, 0.5) / 1.5 = (2/3, 1/3)
  expect_equal(get_w("Alice", "Carol"), (2/3) * (1/3), tolerance = tol)
})

test_that("author_network collaboration with first-author counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", count = "first")

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  ## W1: Alice=1, Bob=0 -> link = 0
  ## W3: Bob=1, Carol=0, Dan=0 -> all links = 0
  ## No edges should exist with first-author counting in co-authorship
  ## because non-first authors have weight 0
  expect_equal(nrow(edges), 0)
})

test_that("author_network with normalization", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", measure = "association")

  expect_true(all(edges$weight >= 0))
  expect_true(all(is.finite(edges$weight)))
})

test_that("author_network coupling type works", {
  d <- make_test_data()
  edges <- author_network(d, "coupling")

  expect_true(is.data.frame(edges))
  expect_true(nrow(edges) > 0)
})
