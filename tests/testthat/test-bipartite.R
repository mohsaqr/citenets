test_that("build_bipartite creates correct sparse matrix", {
  d <- make_test_data()
  B <- build_bipartite(d, "references")

  expect_true(inherits(B, "dgCMatrix"))
  expect_equal(nrow(B), 3)  # 3 works
  expect_equal(ncol(B), 4)  # 4 unique refs: R1, R2, R3, R4
  expect_equal(rownames(B), c("W1", "W2", "W3"))
  expect_equal(colnames(B), c("R1", "R2", "R3", "R4"))

  ## W1 cites R1, R2, R3 (not R4)
  expect_equal(as.numeric(B["W1", ]), c(1, 1, 1, 0))
  ## W2 cites R1, R2, R4 (not R3)
  expect_equal(as.numeric(B["W2", ]), c(1, 1, 0, 1))
  ## W3 cites R2, R3, R4 (not R1)
  expect_equal(as.numeric(B["W3", ]), c(0, 1, 1, 1))
})

test_that("build_bipartite handles min_freq filtering", {
  d <- make_test_data()
  B <- build_bipartite(d, "references", min_freq = 2L)

  ## R1 appears in 2 papers, R2 in 3, R3 in 2, R4 in 2 -> all kept at min_freq=2
  expect_equal(ncol(B), 4)

  B3 <- build_bipartite(d, "references", min_freq = 3L)
  ## Only R2 appears in all 3 papers

  expect_equal(ncol(B3), 1)
  expect_equal(colnames(B3), "R2")
})

test_that("build_bipartite works with authors", {
  d <- make_test_data()
  B <- build_bipartite(d, "authors")

  expect_equal(nrow(B), 3)
  expect_equal(ncol(B), 4)  # ALICE, BOB, CAROL, DAN
  expect_equal(sum(B["W1", ]), 2)  # ALICE + BOB
  expect_equal(sum(B["W3", ]), 3)  # BOB + CAROL + DAN
})

test_that("build_bipartite drops NA, empty, whitespace", {
  d <- data.frame(id = c("W1", "W2"), stringsAsFactors = FALSE)
  d$refs <- list(c("R1", "R2"), c(NA_character_, "", "   "))
  B <- build_bipartite(d, "refs")

  expect_equal(ncol(B), 2)  # only R1, R2
  expect_equal(sum(B["W2", ]), 0)  # nothing valid
})
