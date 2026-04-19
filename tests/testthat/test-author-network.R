test_that("author_network collaboration with full counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration")

  expect_true(is.data.frame(edges))
  expect_true(all(c("from", "to", "weight", "count") %in% names(edges)))

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("ALICE", "BOB"), 1)
  expect_equal(get_w("ALICE", "CAROL"), 1)
  expect_equal(get_w("BOB", "CAROL"), 1)
  expect_equal(get_w("ALICE", "DAN"), 0)
})

test_that("author_network collaboration with harmonic counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", counting = "harmonic")

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  ## W1: Alice (2/3), Bob (1/3) -> co-auth = 2/3 * 1/3 = 2/9
  expect_equal(get_w("ALICE", "BOB"), 2/9, tolerance = tol)

  ## W3: Bob (1/H3), Carol ((1/2)/H3), Dan ((1/3)/H3)
  h3 <- 1 + 1/2 + 1/3
  ## Bob-Carol from W3: (1/h3) * ((1/2)/h3)
  expect_equal(get_w("BOB", "CAROL"), (1/h3) * ((1/2)/h3), tolerance = tol)
})

test_that("author_network collaboration with geometric counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", counting = "geometric")

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  ## W2: Alice (pos 1), Carol (pos 2), n=2
  ## geometric: (1, 0.5) / 1.5 = (2/3, 1/3)
  expect_equal(get_w("ALICE", "CAROL"), (2/3) * (1/3), tolerance = tol)
})

test_that("author_network collaboration with first-author counting", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", counting = "first")

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
  edges <- author_network(d, "collaboration", similarity = "association")

  expect_true(all(edges$weight >= 0))
  expect_true(all(is.finite(edges$weight)))
})

test_that("author_network coupling type works", {
  d <- make_test_data()
  edges <- author_network(d, "coupling")

  expect_true(is.data.frame(edges))
  expect_true(nrow(edges) > 0)
})

## -- attention parameter tests -----------------------------------------------

test_that("attention: all four methods return valid bibnets_network", {
  d <- make_test_data()
  for (att in c("proximity", "lead", "last", "circular")) {
    edges <- author_network(d, attention = att)
    expect_true(inherits(edges, "bibnets_network"), info = att)
    expect_true(all(c("from", "to", "weight", "count") %in% names(edges)), info = att)
    expect_true(all(edges$weight >= 0), info = att)
    expect_true(all(is.finite(edges$weight)), info = att)
  }
})

test_that("attention lead: first author gets highest individual weight", {
  d <- make_test_data()
  # W1: Alice(p1) + Bob(p2), n=2; lead weights = (4,1)/5
  # W(Alice,Bob) = sqrt(4/5) * sqrt(1/5) ... no: B'B gives sum of w_i * w_j
  # W1: Alice weight = 4/5, Bob weight = 1/5 -> edge = 4/5 * 1/5 = 4/25
  # W2: Alice(p1) + Carol(p2) -> same: Alice=4/5, Carol=1/5 -> edge = 4/25
  # W3: Bob(p1)+Carol(p2)+Dan(p3), n=3; (n+1-pos)^2 = (9,4,1)/14
  #   Bob=9/14, Carol=4/14, Dan=1/14
  edges <- author_network(d, attention = "lead")

  get_w <- function(a, b) {
    row <- edges[(edges$from == a & edges$to == b) |
                   (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("ALICE", "BOB"),   (4/5) * (1/5),           tolerance = tol)
  expect_equal(get_w("ALICE", "CAROL"), (4/5) * (1/5),           tolerance = tol)
  expect_equal(get_w("BOB",   "CAROL"), (9/14) * (4/14),         tolerance = tol)
  expect_equal(get_w("BOB",   "DAN"),   (9/14) * (1/14),         tolerance = tol)
  expect_equal(get_w("CAROL", "DAN"),   (4/14) * (1/14),         tolerance = tol)
})

test_that("attention last: last author gets highest individual weight", {
  d <- make_test_data()
  # W1: Alice(p1)+Bob(p2), n=2; pos^2 = (1,4)/5; Alice=1/5, Bob=4/5
  # W2: Alice(p1)+Carol(p2); Alice=1/5, Carol=4/5
  # W3: Bob(p1)+Carol(p2)+Dan(p3), n=3; pos^2=(1,4,9)/14
  edges <- author_network(d, attention = "last")

  get_w <- function(a, b) {
    row <- edges[(edges$from == a & edges$to == b) |
                   (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("ALICE", "BOB"),   (1/5) * (4/5),   tolerance = tol)
  expect_equal(get_w("BOB",   "DAN"),   (1/14) * (9/14), tolerance = tol)
  expect_equal(get_w("CAROL", "DAN"),   (4/14) * (9/14), tolerance = tol)
  ## last author (Dan) gets more weight than lead (Bob) in W3
  expect_true(get_w("CAROL", "DAN") > get_w("BOB", "CAROL"))
})

test_that("attention proximity: center authors weighted more than edges", {
  d <- make_test_data()
  # W3: Bob(p1)+Carol(p2)+Dan(p3), n=3; pmin = (1,2,1)/4
  # Bob=1/4, Carol=2/4, Dan=1/4
  edges <- author_network(d, attention = "proximity")

  get_w <- function(a, b) {
    row <- edges[(edges$from == a & edges$to == b) |
                   (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("BOB",   "CAROL"), (1/4) * (2/4), tolerance = tol)
  expect_equal(get_w("CAROL", "DAN"),   (2/4) * (1/4), tolerance = tol)
  expect_equal(get_w("BOB",   "DAN"),   (1/4) * (1/4), tolerance = tol)
  ## center pair outweighs edge pair
  expect_true(get_w("BOB", "CAROL") > get_w("BOB", "DAN"))
})

test_that("attention circular: first and last outweigh middle", {
  d <- make_test_data()
  # W3: Bob(p1)+Carol(p2)+Dan(p3), n=3; pmax = (3,2,3)/8
  # Bob=3/8, Carol=2/8, Dan=3/8
  edges <- author_network(d, attention = "circular")

  get_w <- function(a, b) {
    row <- edges[(edges$from == a & edges$to == b) |
                   (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("BOB",   "DAN"),   (3/8) * (3/8), tolerance = tol)
  expect_equal(get_w("BOB",   "CAROL"), (3/8) * (2/8), tolerance = tol)
  ## first-last edge outweighs first-middle
  expect_true(get_w("BOB", "DAN") > get_w("BOB", "CAROL"))
})

test_that("attention: invalid method gives clear error", {
  d <- make_test_data()
  expect_error(author_network(d, attention = "bad"), "attention")
})

test_that("attention: works on keyword_network", {
  d <- make_test_data()
  edges <- keyword_network(d, attention = "lead")
  expect_s3_class(edges, "bibnets_network")
  expect_true(nrow(edges) > 0)
})

test_that("attention: works on country_network", {
  d <- make_test_data()
  d$countries <- list(c("FI", "SE"), c("FI", "DE"), c("SE", "DE", "FI"))
  edges <- country_network(d, attention = "circular")
  expect_s3_class(edges, "bibnets_network")
  expect_true(nrow(edges) > 0)
})

test_that("attention: works on institution_network", {
  d <- make_test_data()
  d$affiliations <- list(c("MIT", "Harvard"), c("MIT", "Stanford"),
                         c("Harvard", "Stanford", "MIT"))
  edges <- institution_network(d, attention = "proximity")
  expect_s3_class(edges, "bibnets_network")
  expect_true(nrow(edges) > 0)
})
