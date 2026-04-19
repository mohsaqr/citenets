## Tests for prune() — threshold and top_n pruning

make_prune_edges <- function() {
  data.frame(
    from   = c("A","A","A","B","B","C"),
    to     = c("B","C","D","C","D","D"),
    weight = c(5,  1,  2,  4,  1,  3),
    stringsAsFactors = FALSE
  )
}

## ── threshold ──────────────────────────────────────────────────────────────

test_that("prune threshold keeps edges at or above value", {
  edges <- make_prune_edges()
  p <- prune(edges, threshold = 3)
  expect_true(all(p$weight >= 3))
  expect_equal(nrow(p), 3)   # weights 5, 4, 3
})

test_that("prune threshold=0 keeps everything", {
  edges <- make_prune_edges()
  p <- prune(edges, threshold = 0)
  expect_equal(nrow(p), nrow(edges))
})

test_that("prune threshold=6 drops everything", {
  edges <- make_prune_edges()
  p <- prune(edges, threshold = 6)
  expect_equal(nrow(p), 0)
})

test_that("prune threshold result sorted descending by weight", {
  edges <- make_prune_edges()
  p <- prune(edges, threshold = 0)
  expect_true(all(diff(p$weight) <= 0))
})

## ── top_n ──────────────────────────────────────────────────────────────────

test_that("prune top_n keeps correct edges", {
  edges <- make_prune_edges()
  p <- prune(edges, top_n = 2)

  expect_equal(nrow(p), 4)
  expect_true(all(p$weight >= 2))

  has_edge <- function(f, t) {
    any((p$from == f & p$to == t) | (p$from == t & p$to == f))
  }
  expect_true(has_edge("A", "B"))
  expect_true(has_edge("A", "D"))
  expect_true(has_edge("B", "C"))
  expect_true(has_edge("C", "D"))
  expect_false(has_edge("A", "C"))
  expect_false(has_edge("B", "D"))
})

test_that("prune top_n with n >= max degree keeps everything", {
  edges <- make_prune_edges()
  p <- prune(edges, top_n = 10)
  expect_equal(nrow(p), nrow(edges))
})

test_that("prune top_n=1 keeps only the strongest edge per node", {
  edges <- make_prune_edges()
  p <- prune(edges, top_n = 1)

  has_edge <- function(f, t) {
    any((p$from == f & p$to == t) | (p$from == t & p$to == f))
  }
  expect_true(has_edge("A", "B"))
  expect_true(has_edge("B", "C"))
})

test_that("prune top_n result sorted descending by weight", {
  edges <- make_prune_edges()
  p <- prune(edges, top_n = 2)
  expect_true(all(diff(p$weight) <= 0))
})

## ── input preservation ─────────────────────────────────────────────────────

test_that("prune preserves extra columns", {
  edges <- make_prune_edges()
  edges$count <- 1L

  p <- prune(edges, threshold = 3)
  expect_true("count" %in% names(p))
})

test_that("prune returns empty data frame on empty input", {
  empty <- data.frame(from = character(0), to = character(0),
                       weight = numeric(0), stringsAsFactors = FALSE)
  p <- prune(empty, threshold = 1)
  expect_equal(nrow(p), 0)
})

test_that("prune errors when neither threshold nor top_n given", {
  edges <- make_prune_edges()
  expect_error(prune(edges), "must be specified")
})

test_that("prune errors when both threshold and top_n given", {
  edges <- make_prune_edges()
  expect_error(prune(edges, threshold = 2, top_n = 3), "Only one")
})
