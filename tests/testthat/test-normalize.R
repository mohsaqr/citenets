test_that("normalize with 'none' returns input with zeroed diagonal", {
  A <- matrix(c(10, 3, 1, 3, 8, 2, 1, 2, 5), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  result <- normalize(A, "none")
  expect_equal(as.matrix(result), A)
})

test_that("association strength: s_ij = c_ij / (w_i * w_j)", {
  A <- matrix(c(10, 3, 1, 3, 8, 2, 1, 2, 5), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  S <- as.matrix(normalize(A, "association"))

  ## s_ab = 3 / (10 * 8) = 0.0375
  expect_equal(S["a", "b"], 3 / (10 * 8), tolerance = tol)
  ## s_ac = 1 / (10 * 5) = 0.02
  expect_equal(S["a", "c"], 1 / (10 * 5), tolerance = tol)
  ## s_bc = 2 / (8 * 5) = 0.05
  expect_equal(S["b", "c"], 2 / (8 * 5), tolerance = tol)
  ## Diagonal should be zero
  expect_equal(S["a", "a"], 0)
})

test_that("cosine normalization: s_ij = c_ij / sqrt(w_i * w_j)", {
  A <- matrix(c(10, 3, 1, 3, 8, 2, 1, 2, 5), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  S <- as.matrix(normalize(A, "cosine"))

  expect_equal(S["a", "b"], 3 / sqrt(10 * 8), tolerance = tol)
  expect_equal(S["a", "c"], 1 / sqrt(10 * 5), tolerance = tol)
  expect_equal(S["b", "c"], 2 / sqrt(8 * 5), tolerance = tol)
})

test_that("jaccard normalization: s_ij = c_ij / (w_i + w_j - c_ij)", {
  A <- matrix(c(10, 3, 1, 3, 8, 2, 1, 2, 5), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  S <- as.matrix(normalize(A, "jaccard"))

  expect_equal(S["a", "b"], 3 / (10 + 8 - 3), tolerance = tol)
  expect_equal(S["a", "c"], 1 / (10 + 5 - 1), tolerance = tol)
  expect_equal(S["b", "c"], 2 / (8 + 5 - 2), tolerance = tol)
})

test_that("inclusion normalization: s_ij = c_ij / min(w_i, w_j)", {
  A <- matrix(c(10, 3, 1, 3, 8, 2, 1, 2, 5), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  S <- as.matrix(normalize(A, "inclusion"))

  expect_equal(S["a", "b"], 3 / min(10, 8), tolerance = tol)
  expect_equal(S["a", "c"], 1 / min(10, 5), tolerance = tol)
  expect_equal(S["b", "c"], 2 / min(8, 5), tolerance = tol)
})

test_that("equivalence normalization: s_ij = c_ij^2 / (w_i * w_j)", {
  A <- matrix(c(10, 3, 1, 3, 8, 2, 1, 2, 5), nrow = 3,
              dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  S <- as.matrix(normalize(A, "equivalence"))

  expect_equal(S["a", "b"], 3^2 / (10 * 8), tolerance = tol)
  expect_equal(S["a", "c"], 1^2 / (10 * 5), tolerance = tol)
  expect_equal(S["b", "c"], 2^2 / (8 * 5), tolerance = tol)
})

test_that("normalize handles zero diagonal gracefully", {
  A <- matrix(c(0, 2, 2, 0), nrow = 2,
              dimnames = list(c("x", "y"), c("x", "y")))
  S <- as.matrix(normalize(A, "association"))
  ## With zero diagonal, all entries should be 0 (no division by zero errors)
  expect_true(all(is.finite(S)))
})
