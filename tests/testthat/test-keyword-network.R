test_that("keyword_network returns correct co-occurrence", {
  d <- make_test_data()
  edges <- keyword_network(d, threshold = 0)

  get_w <- function(a, b) {
    row <- edges[
      (edges$from == a & edges$to == b) |
      (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("ML", "DL"), 1)
  expect_equal(get_w("ML", "NLP"), 1)
  expect_equal(get_w("DL", "NLP"), 1)
  expect_equal(get_w("ML", "CV"), 0)
})
