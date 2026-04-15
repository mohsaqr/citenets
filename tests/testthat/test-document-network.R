test_that("document_network coupling returns correct edges", {
  d <- make_test_data()
  edges <- document_network(d, "coupling")

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  ## All pairs share 2 refs
  expect_equal(get_w("W1", "W2"), 2)
  expect_equal(get_w("W1", "W3"), 2)
  expect_equal(get_w("W2", "W3"), 2)
})

test_that("document_network citation returns directed internal citations", {
  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$references <- list(c("W2", "W3", "R1"), c("W3"), c("R2"))

  edges <- document_network(d, "citation")
  expect_true(any(edges$from == "W1" & edges$to == "W2"))
  expect_true(any(edges$from == "W1" & edges$to == "W3"))
  expect_false("R1" %in% c(edges$from, edges$to))
})

test_that("document_network with cosine gives coupling angle", {
  d <- make_test_data()
  edges <- document_network(d, "coupling", measure = "cosine")

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  ## W1-W2: 2 shared out of 3 each -> 2/sqrt(3*3) = 2/3
  expect_equal(get_w("W1", "W2"), 2/3, tolerance = tol)
})
