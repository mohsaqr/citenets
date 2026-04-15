## Cross-validation: counting methods against hand-computed reference values
## Every formula verified independently from the implementation

test_that("harmonic weights match Hagen 2008 Table 1", {
  ## Table 1 from Hagen (2008): harmonic credit for 3-author paper
  ## 0.5455, 0.2727, 0.1818
  w <- author_weights(3, "harmonic")
  expect_equal(w[1], 6/11, tolerance = 1e-4)   # 0.5454...
  expect_equal(w[2], 3/11, tolerance = 1e-4)   # 0.2727...
  expect_equal(w[3], 2/11, tolerance = 1e-4)   # 0.1818...

  ## 6-author paper: Hagen Table 1
  w6 <- author_weights(6, "harmonic")
  h6 <- sum(1 / 1:6)
  for (i in 1:6) {
    expect_equal(w6[i], (1/i) / h6, tolerance = 1e-10,
                 info = sprintf("Harmonic n=6, pos=%d", i))
  }
})

test_that("arithmetic weights match proportional counting formula", {
  ## Arithmetic: w_i = (N - i + 1) / sum(1:N)
  ## For N=4: (4, 3, 2, 1) / 10 = (0.4, 0.3, 0.2, 0.1)
  w <- author_weights(4, "arithmetic")
  expect_equal(w, c(0.4, 0.3, 0.2, 0.1), tolerance = 1e-10)

  ## For N=5: (5, 4, 3, 2, 1) / 15
  w5 <- author_weights(5, "arithmetic")
  expect_equal(w5, c(5, 4, 3, 2, 1) / 15, tolerance = 1e-10)
})

test_that("geometric weights match 0.5^(i-1) formula", {
  ## Geometric: w_i = 0.5^(i-1) / sum(0.5^(0:N-1))
  ## For N=3: (1, 0.5, 0.25) / 1.75
  w <- author_weights(3, "geometric")
  raw <- c(1, 0.5, 0.25)
  expect_equal(w, raw / sum(raw), tolerance = 1e-10)

  ## For N=5
  w5 <- author_weights(5, "geometric")
  raw5 <- 0.5^(0:4)
  expect_equal(w5, raw5 / sum(raw5), tolerance = 1e-10)
})

test_that("adaptive_geometric: first/last ratio = N", {
  ## Liu & Fang 2023: w_1 / w_N = N
  for (n in 2:10) {
    w <- author_weights(n, "adaptive_geometric")
    expect_equal(w[1] / w[n], n, tolerance = 0.01,
                 info = sprintf("Adaptive geometric ratio for n=%d", n))
    expect_equal(sum(w), 1, tolerance = 1e-10)
  }
})

test_that("golden ratio weights use phi = (1+sqrt(5))/2", {
  phi <- (1 + sqrt(5)) / 2

  w3 <- author_weights(3, "golden")
  raw <- c(phi^2, phi^1, phi^0)
  expect_equal(w3, raw / sum(raw), tolerance = 1e-10)

  ## Golden ratio property: consecutive ratio should approach phi
  w10 <- author_weights(10, "golden")
  for (i in 1:9) {
    expect_equal(w10[i] / w10[i + 1], phi, tolerance = 1e-10,
                 info = sprintf("Golden ratio at position %d", i))
  }
})

test_that("first_last gives elevated weight to positions 1 and N", {
  ## N=4, first_last_weight=2 (default)
  w <- author_weights(4, "first_last")
  expect_equal(sum(w), 1, tolerance = 1e-10)
  expect_equal(w[1], w[4], tolerance = 1e-10)  # first == last
  expect_true(w[1] > w[2])                      # elevated > middle
  expect_equal(w[2], w[3], tolerance = 1e-10)  # middle are equal

  ## N=2: should be 50/50
  w2 <- author_weights(2, "first_last")
  expect_equal(w2, c(0.5, 0.5), tolerance = 1e-10)
})

test_that("position_weighted uses custom weight vector", {
  custom_w <- c(1.0, 0.8, 0.6, 0.4)

  w4 <- author_weights(4, "position_weighted", position_weights = custom_w)
  expect_equal(w4, custom_w / sum(custom_w), tolerance = 1e-10)

  ## N=6: positions 5 and 6 should get last weight (0.4)
  w6 <- author_weights(6, "position_weighted", position_weights = custom_w)
  raw6 <- c(1.0, 0.8, 0.6, 0.4, 0.4, 0.4)
  expect_equal(w6, raw6 / sum(raw6), tolerance = 1e-10)
})

test_that("network weights via multiplication match expected products", {
  d <- data.frame(id = c("W1"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"))

  ## Harmonic: A=6/11, B=3/11, C=2/11
  ## A-B link = (6/11)*(3/11) = 18/121
  ## A-C link = (6/11)*(2/11) = 12/121
  ## B-C link = (3/11)*(2/11) = 6/121
  edges <- author_network(d, "collaboration", count = "harmonic")

  get_w <- function(a, b) {
    row <- edges[(edges$from == a & edges$to == b) |
                 (edges$from == b & edges$to == a), ]
    if (nrow(row) == 0) return(0)
    row$weight[1]
  }

  expect_equal(get_w("A", "B"), 18/121, tolerance = 1e-10)
  expect_equal(get_w("A", "C"), 12/121, tolerance = 1e-10)
  expect_equal(get_w("B", "C"), 6/121,  tolerance = 1e-10)
})

test_that("fractional counting for references: Perianes-Rodriguez 1/(n-1)", {
  ## Single paper with 4 refs, fractional co-citation counting
  ## Symmetric network: w = 1/(n-1) = 1/3 per pair per paper
  ## Each reference's total co-citation from this paper = 1
  d <- data.frame(id = "W1", stringsAsFactors = FALSE)
  d$references <- list(c("R1", "R2", "R3", "R4"))

  edges <- reference_network(d, count = "fractional", threshold = 0)

  ## All pairs should have weight 1/(4-1) = 1/3
  expect_true(all(abs(edges$weight - 1/3) < 1e-10))
})

test_that("paper-level fractional: Batagelj total per paper = 1", {
  ## Paper with 4 refs: 6 co-citation pairs, each = 2/(4*3) = 1/6
  ## Total = 6 * 1/6 = 1
  d <- data.frame(id = "W1", stringsAsFactors = FALSE)
  d$references <- list(c("R1", "R2", "R3", "R4"))

  edges <- reference_network(d, count = "paper", threshold = 0)

  ## Sum of all edge weights = 1
  expect_equal(sum(edges$weight), 1, tolerance = 1e-10)

  ## Each of the 6 edges = 1/6
  expect_true(all(abs(edges$weight - 1/6) < 1e-10))
})

test_that("author fractional counting: link weight = 1/(n-1) per shared paper", {
  ## Single paper, 4 authors, Perianes-Rodriguez fractional counting
  ## apply_counting symmetric: w = 1/(n-1) = 1/3 per link per paper
  d <- data.frame(id = "W1", stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C", "D"))

  edges <- author_network(d, "collaboration", count = "fractional")

  ## 6 pairs, each weight = 1/(4-1) = 1/3
  expect_true(all(abs(edges$weight - 1/3) < 1e-10))
})
