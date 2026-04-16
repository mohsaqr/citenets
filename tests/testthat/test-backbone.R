## Tests for backbone() — Serrano disparity filter

## Hub graph: A is a hub with 3 weak edges and 1 strong one
## A-B(10), A-C(1), A-D(1), B-C(8), C-D(1)
make_backbone_edges <- function() {
  data.frame(
    from   = c("A", "A", "A", "B", "C"),
    to     = c("B", "C", "D", "C", "D"),
    weight = c(10,   1,   1,   8,   1),
    stringsAsFactors = FALSE
  )
}

test_that("backbone returns correct edges for hub graph at alpha 0.05", {
  edges <- make_backbone_edges()
  b <- backbone(edges, alpha = 0.05)

  ## Expected: A-B (alpha from A ≈ 0.028 < 0.05) and B-C (alpha from C = 0.04 < 0.05)
  ## All other edges have min-alpha above 0.05
  expect_equal(nrow(b), 2)
  expect_true(all(c("from", "to", "weight", "alpha") %in% names(b)))

  ## A-B must be present
  ab <- b[(b$from == "A" & b$to == "B") | (b$from == "B" & b$to == "A"), ]
  expect_equal(nrow(ab), 1)
  expect_equal(ab$weight, 10)

  ## B-C must be present
  bc <- b[(b$from == "B" & b$to == "C") | (b$from == "C" & b$to == "B"), ]
  expect_equal(nrow(bc), 1)
  expect_equal(bc$weight, 8)
})

test_that("backbone alpha values are correct", {
  edges <- make_backbone_edges()
  b <- backbone(edges, alpha = 0.9999)   # keep everything

  expect_equal(nrow(b), nrow(edges))

  ## For A-B edge: from A: s=12, k=3, alpha=(1-10/12)^2 = (1/6)^2 ≈ 0.0278
  ##               from B: s=18, k=2, alpha=(1-10/18)^1 = 8/18 ≈ 0.444
  ##               min = 0.0278
  ab <- b[(b$from == "A" & b$to == "B") | (b$from == "B" & b$to == "A"), ]
  expect_equal(ab$alpha, (1 - 10/12)^2, tolerance = 1e-10)
})

test_that("backbone returns empty data frame when no edges pass filter", {
  ## All weak, tiny graph — nothing passes strict alpha
  edges <- data.frame(
    from   = c("A", "B"),
    to     = c("B", "C"),
    weight = c(1,   1),
    stringsAsFactors = FALSE
  )
  ## Nodes with degree 1 always get alpha=0, so both edges are kept
  b <- backbone(edges, alpha = 0.01)
  expect_equal(nrow(b), 2)
})

test_that("backbone always keeps edges from degree-1 nodes", {
  ## A is a leaf (degree 1): its alpha = 0, so A-B is always kept
  edges <- data.frame(
    from   = c("A", "B", "B"),
    to     = c("B", "C", "D"),
    weight = c(1,   100,  100),
    stringsAsFactors = FALSE
  )
  b <- backbone(edges, alpha = 0.001)
  ## A-B should be kept (A has degree 1 → alpha=0 from A's side)
  ab <- b[(b$from == "A" & b$to == "B") | (b$from == "B" & b$to == "A"), ]
  expect_equal(nrow(ab), 1)
})

test_that("backbone returns input columns unchanged plus alpha", {
  edges <- make_backbone_edges()
  edges$count  <- 1L
  edges$shared <- 1L
  b <- backbone(edges, alpha = 0.9999)  # keep all
  expect_true("count"  %in% names(b))
  expect_true("shared" %in% names(b))
  expect_true("alpha"  %in% names(b))
})

test_that("backbone handles empty edge list", {
  empty <- data.frame(from = character(0), to = character(0),
                       weight = numeric(0), stringsAsFactors = FALSE)
  b <- backbone(empty, alpha = 0.05)
  expect_equal(nrow(b), 0)
  expect_true("alpha" %in% names(b))
})

test_that("backbone result is sorted by weight descending", {
  edges <- make_backbone_edges()
  b <- backbone(edges, alpha = 0.9999)
  expect_true(all(diff(b$weight) <= 0))
})

test_that("backbone rejects bad alpha values", {
  edges <- make_backbone_edges()
  expect_error(backbone(edges, alpha = 0))
  expect_error(backbone(edges, alpha = 1))
  expect_error(backbone(edges, alpha = -0.1))
  expect_error(backbone(edges, alpha = 1.1))
})
