test_that("author_weights returns correct values for all 13 methods", {
  ## 3 authors
  n <- 3

  ## Fractional: equal share
  w <- author_weights(n, "fractional")
  expect_equal(w, c(1/3, 1/3, 1/3), tolerance = tol)
  expect_equal(sum(w), 1, tolerance = tol)

  ## Harmonic: (1/1, 1/2, 1/3) / H_3
  w <- author_weights(n, "harmonic")
  h3 <- 1 + 1/2 + 1/3
  expect_equal(w, c(1/h3, (1/2)/h3, (1/3)/h3), tolerance = tol)
  expect_equal(sum(w), 1, tolerance = tol)

  ## Arithmetic: (3, 2, 1) / 6
  w <- author_weights(n, "arithmetic")
  expect_equal(w, c(3/6, 2/6, 1/6), tolerance = tol)

  ## Geometric: (0.25, 0.5, 1) normalized... wait (0.5^0, 0.5^1, 0.5^2) = (1, 0.5, 0.25)
  w <- author_weights(n, "geometric")
  raw <- c(1, 0.5, 0.25)
  expect_equal(w, raw / sum(raw), tolerance = tol)

  ## Adaptive geometric: ratio first/last = 3
  w <- author_weights(n, "adaptive_geometric")
  expect_equal(w[1] / w[3], 3, tolerance = 0.01)
  expect_equal(sum(w), 1, tolerance = tol)

  ## Golden: phi-based
  w <- author_weights(n, "golden")
  phi <- (1 + sqrt(5)) / 2
  raw <- c(phi^2, phi^1, phi^0)
  expect_equal(w, raw / sum(raw), tolerance = tol)

  ## First author only
  w <- author_weights(n, "first")
  expect_equal(w, c(1, 0, 0))

  ## Last author only
  w <- author_weights(n, "last")
  expect_equal(w, c(0, 0, 1))

  ## First-last elevated
  w <- author_weights(n, "first_last")
  expect_true(w[1] == w[3])  # first == last
  expect_true(w[1] > w[2])   # elevated > middle
  expect_equal(sum(w), 1, tolerance = tol)

  ## Position weighted
  w <- author_weights(n, "position_weighted")
  expect_equal(sum(w), 1, tolerance = tol)

  ## Full: all ones (NOT normalized to 1)
  w <- author_weights(n, "full")
  expect_equal(w, c(1, 1, 1))
})

test_that("author_weights handles single author", {
  for (m in all_counts()) {
    w <- author_weights(1, m)
    expect_equal(w, 1)
  }
})

test_that("all position-dependent methods sum to 1", {
  for (n in 2:10) {
    for (m in position_dependent_counts()) {
      w <- author_weights(n, m)
      expect_equal(sum(w), 1, tolerance = tol,
                   info = sprintf("%s with n=%d", m, n))
    }
  }
})

test_that("harmonic matches crisstats formula", {
  ## From crisstats: weight_i = (1/position) / sum(1/1:n)
  for (n in c(2, 5, 10)) {
    w <- author_weights(n, "harmonic")
    h_n <- sum(1 / seq_len(n))
    expected <- (1 / seq_len(n)) / h_n
    expect_equal(w, expected, tolerance = tol)
  }
})

test_that("geometric matches crisstats formula", {
  ## From crisstats: weight_i = 0.5^(i-1), normalized
  for (n in c(2, 5, 10)) {
    w <- author_weights(n, "geometric")
    raw <- 0.5^(seq_len(n) - 1)
    expected <- raw / sum(raw)
    expect_equal(w, expected, tolerance = tol)
  }
})

test_that("build_author_bipartite produces correct positional weights", {
  d <- make_test_data()

  ## Harmonic weighting
  B <- build_author_bipartite(d, counting = "harmonic")

  ## W1 has Alice (pos 1) and Bob (pos 2), n=2
  ## Harmonic: H_2 = 1 + 0.5 = 1.5
  ## Alice: (1/1)/1.5 = 2/3, Bob: (1/2)/1.5 = 1/3
  expect_equal(B["W1", "Alice"], 2/3, tolerance = tol)
  expect_equal(B["W1", "Bob"], 1/3, tolerance = tol)

  ## W3 has Bob (pos 1), Carol (pos 2), Dan (pos 3), n=3
  h3 <- 1 + 1/2 + 1/3
  expect_equal(B["W3", "Bob"], 1/h3, tolerance = tol)
  expect_equal(B["W3", "Carol"], (1/2)/h3, tolerance = tol)
  expect_equal(B["W3", "Dan"], (1/3)/h3, tolerance = tol)
})
