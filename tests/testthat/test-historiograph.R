## Tests for local_citations() and historiograph()

## Data where papers cite each other:
## W1 (2018) ← W2 (2019), W3 (2020)
## W2 (2019) ← W3 (2020), W4 (2021)
## W3 (2020) ← W4 (2021)
## W4 (2021) ← nobody
make_citation_data <- function() {
  d <- data.frame(
    id    = c("W1", "W2", "W3", "W4"),
    year  = c(2018L, 2019L, 2020L, 2021L),
    title = c("T1", "T2", "T3", "T4"),
    stringsAsFactors = FALSE
  )
  d$references <- list(
    character(0),        # W1 cites nobody
    c("W1"),             # W2 cites W1
    c("W1", "W2"),       # W3 cites W1 and W2
    c("W2", "W3")        # W4 cites W2 and W3
  )
  d
}

## ── local_citations ────────────────────────────────────────────────────────

test_that("local_citations returns correct LCS counts", {
  d <- make_citation_data()
  lcs <- local_citations(d)

  expect_true(is.data.frame(lcs))
  expect_true(all(c("id", "lcs") %in% names(lcs)))

  get_lcs <- function(id) lcs$lcs[lcs$id == id]

  expect_equal(get_lcs("W1"), 2L)  # cited by W2, W3
  expect_equal(get_lcs("W2"), 2L)  # cited by W3, W4
  expect_equal(get_lcs("W3"), 1L)  # cited by W4
  expect_equal(get_lcs("W4"), 0L)  # not cited
})

test_that("local_citations is sorted by lcs descending", {
  d <- make_citation_data()
  lcs <- local_citations(d)
  expect_true(all(diff(lcs$lcs) <= 0))
})

test_that("local_citations includes year when present", {
  d <- make_citation_data()
  lcs <- local_citations(d)
  expect_true("year" %in% names(lcs))
  expect_true(all(is.integer(lcs$year)))
})

test_that("local_citations includes gcs when cited_by_count present", {
  d <- make_citation_data()
  d$cited_by_count <- c(100L, 50L, 20L, 5L)
  lcs <- local_citations(d)
  expect_true("gcs" %in% names(lcs))
})

test_that("local_citations works when no internal citations exist", {
  d <- make_test_data()  # W1-W3 only cite external refs R1-R4
  lcs <- local_citations(d)
  expect_equal(nrow(lcs), 3)
  expect_true(all(lcs$lcs == 0L))
})

test_that("local_citations handles missing references gracefully", {
  d <- make_citation_data()
  d$references <- lapply(d$references, function(x) c(x, NA_character_))
  lcs <- local_citations(d)
  expect_equal(nrow(lcs), 4)
})

## ── historiograph ──────────────────────────────────────────────────────────

test_that("historiograph returns list with nodes and edges", {
  d <- make_citation_data()
  h <- historiograph(d, n = 3)

  expect_true(is.list(h))
  expect_true(all(c("nodes", "edges") %in% names(h)))
})

test_that("historiograph nodes are top-n by LCS", {
  d <- make_citation_data()
  h <- historiograph(d, n = 3, min_lcs = 1)

  ## Top 3 by LCS: W1(2), W2(2), W3(1); W4 has lcs=0
  expect_equal(nrow(h$nodes), 3)
  expect_false("W4" %in% h$nodes$id)
})

test_that("historiograph edges have correct structure", {
  d <- make_citation_data()
  h <- historiograph(d, n = 3, min_lcs = 1)

  expect_true(all(c("from", "to", "year_from", "year_to") %in% names(h$edges)))
  expect_true(is.integer(h$edges$year_from))
  expect_true(is.integer(h$edges$year_to))
})

test_that("historiograph edges include citations from non-top documents", {
  d <- make_citation_data()
  ## Top-3 nodes: W1, W2, W3
  ## W4 is NOT in top-3 (lcs=0) but cites W2 and W3 which ARE in top-3
  h <- historiograph(d, n = 3, min_lcs = 1)

  ## W4→W2 and W4→W3 should appear (W4 is a citing doc, not a node)
  has_edge <- function(f, t) any(h$edges$from == f & h$edges$to == t)
  expect_true(has_edge("W4", "W2"))
  expect_true(has_edge("W4", "W3"))
  ## W3→W2 too
  expect_true(has_edge("W3", "W2"))
})

test_that("historiograph min_lcs filters correctly", {
  d <- make_citation_data()
  ## min_lcs = 2: only W1(2) and W2(2) qualify
  h <- historiograph(d, n = 30, min_lcs = 2)
  expect_equal(nrow(h$nodes), 2)
  expect_true(all(h$nodes$id %in% c("W1", "W2")))
})

test_that("historiograph returns empty list when min_lcs too high", {
  d <- make_citation_data()
  h <- historiograph(d, n = 10, min_lcs = 99)
  expect_equal(nrow(h$nodes), 0)
  expect_equal(nrow(h$edges), 0)
})

test_that("historiograph requires year column", {
  d <- make_citation_data()
  d$year <- NULL
  expect_error(historiograph(d))
})

test_that("historiograph edges are sorted by year_from", {
  d <- make_citation_data()
  h <- historiograph(d, n = 4, min_lcs = 0)
  if (nrow(h$edges) > 1) {
    expect_true(all(diff(h$edges$year_from) >= 0))
  }
})
