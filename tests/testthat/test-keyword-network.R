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

  expect_equal(get_w("ml", "dl"), 1)
  expect_equal(get_w("ml", "nlp"), 1)
  expect_equal(get_w("dl", "nlp"), 1)
  expect_equal(get_w("ml", "cv"), 0)
})
