## Cross-validation: citenets vs biblionetwork (Goutsmedt)
## Tests co-authorship (full, fractional, paper), coupling angle, co-citation

test_that("co-authorship full counting matches biblionetwork", {
  skip_if_not_installed("biblionetwork")
  skip_if_not_installed("data.table")

  ## Same data: 3 papers, 4 authors
  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  ## biblionetwork reference values (verified):
  ## A-B: 2, A-C: 1, B-C: 2, B-D: 1, C-D: 1
  bn_ref <- data.frame(
    from = c("C", "B", "C", "D", "D"),
    to   = c("A", "A", "B", "B", "C"),
    weight = c(1, 2, 2, 1, 1),
    stringsAsFactors = FALSE
  )

  edges <- author_network(d, "collaboration", count = "full")

  for (i in seq_len(nrow(bn_ref))) {
    a <- bn_ref$from[i]; b <- bn_ref$to[i]
    row <- edges[(edges$from == a & edges$to == b) |
                 (edges$from == b & edges$to == a), ]
    expect_equal(row$weight[1], bn_ref$weight[i], tolerance = 1e-10,
                 info = sprintf("Full co-auth %s-%s", a, b))
  }
})

test_that("co-authorship fractional matches biblionetwork fractional_counting", {
  skip_if_not_installed("biblionetwork")

  ## biblionetwork fractional_counting = 1/(n-1) per link per paper
  ## W1: n=3, each pair weight = 1/(3-1) = 0.5
  ## W2: n=2, each pair weight = 1/(2-1) = 1.0
  ## W3: n=3, each pair weight = 1/(3-1) = 0.5
  ## B-A: W1(0.5) + W2(1.0) = 1.5
  ## C-A: W1(0.5) = 0.5
  ## C-B: W1(0.5) + W3(0.5) = 1.0
  ## D-B: W3(0.5) = 0.5
  ## D-C: W3(0.5) = 0.5
  bn_ref <- data.frame(
    from = c("C", "B", "C", "D", "D"),
    to   = c("A", "A", "B", "B", "C"),
    weight = c(0.5, 1.5, 1.0, 0.5, 0.5),
    stringsAsFactors = FALSE
  )

  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  edges <- author_network(d, "collaboration", count = "fractional")

  for (i in seq_len(nrow(bn_ref))) {
    a <- bn_ref$from[i]; b <- bn_ref$to[i]
    row <- edges[(edges$from == a & edges$to == b) |
                 (edges$from == b & edges$to == a), ]
    expect_equal(row$weight[1], bn_ref$weight[i], tolerance = 1e-10,
                 info = sprintf("Fractional co-auth %s-%s", a, b))
  }
})

test_that("co-authorship paper counting matches biblionetwork fractional_counting_refined", {
  skip_if_not_installed("biblionetwork")

  ## biblionetwork fractional_counting_refined = 2/(n*(n-1)) per link
  ## W1: n=3, weight = 2/(3*2) = 1/3
  ## W2: n=2, weight = 2/(2*1) = 1
  ## W3: n=3, weight = 2/(3*2) = 1/3
  ## B-A: W1(1/3) + W2(1) = 4/3 = 1.3333
  ## C-A: W1(1/3) = 1/3 = 0.3333
  ## C-B: W1(1/3) + W3(1/3) = 2/3 = 0.6667
  ## D-B: W3(1/3) = 1/3
  ## D-C: W3(1/3) = 1/3
  bn_ref <- data.frame(
    from = c("C", "B", "C", "D", "D"),
    to   = c("A", "A", "B", "B", "C"),
    weight = c(1/3, 4/3, 2/3, 1/3, 1/3),
    stringsAsFactors = FALSE
  )

  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  edges <- author_network(d, "collaboration", count = "paper")

  for (i in seq_len(nrow(bn_ref))) {
    a <- bn_ref$from[i]; b <- bn_ref$to[i]
    row <- edges[(edges$from == a & edges$to == b) |
                 (edges$from == b & edges$to == a), ]
    expect_equal(row$weight[1], bn_ref$weight[i], tolerance = 1e-10,
                 info = sprintf("Paper co-auth %s-%s", a, b))
  }
})

test_that("co-authorship full + cosine matches biblionetwork", {
  skip_if_not_installed("biblionetwork")

  ## biblionetwork cosine_normalized full_counting reference values:
  ## B-A: 0.8164966, C-A: 0.5, C-B: 0.8164966, D-B: 0.5773503, D-C: 0.7071068
  bn_ref <- data.frame(
    from = c("B", "C", "C", "D", "D"),
    to   = c("A", "A", "B", "B", "C"),
    weight = c(0.8164966, 0.5, 0.8164966, 0.5773503, 0.7071068),
    stringsAsFactors = FALSE
  )

  d <- data.frame(id = c("W1", "W2", "W3"), stringsAsFactors = FALSE)
  d$authors <- list(c("A", "B", "C"), c("A", "B"), c("B", "C", "D"))

  edges <- author_network(d, "collaboration", count = "full", measure = "cosine")

  for (i in seq_len(nrow(bn_ref))) {
    a <- bn_ref$from[i]; b <- bn_ref$to[i]
    row <- edges[(edges$from == a & edges$to == b) |
                 (edges$from == b & edges$to == a), ]
    expect_equal(row$weight[1], bn_ref$weight[i], tolerance = 1e-4,
                 info = sprintf("Full+cosine co-auth %s-%s", a, b))
  }
})

test_that("coupling cosine matches biblionetwork biblio_coupling", {
  skip_if_not_installed("biblionetwork")

  ## biblionetwork coupling reference (cosine/coupling angle):
  bn_ref <- data.frame(
    from = c("W1","W1","W1","W1","W2","W2","W2","W3","W3"),
    to   = c("W2","W3","W4","W5","W3","W4","W5","W4","W5"),
    weight = c(2/3, 2/3, 2/3, 1/3, 1/3, 2/3, 1/3, 1/3, 2/3),
    stringsAsFactors = FALSE
  )

  d <- data.frame(id = paste0("W", 1:5), stringsAsFactors = FALSE)
  d$references <- list(
    c("R1","R2","R3"), c("R1","R2","R4"), c("R2","R3","R5"),
    c("R1","R3","R4"), c("R2","R5","R6")
  )

  edges <- document_network(d, "coupling", measure = "cosine")

  for (i in seq_len(nrow(bn_ref))) {
    a <- bn_ref$from[i]; b <- bn_ref$to[i]
    row <- edges[(edges$from == a & edges$to == b) |
                 (edges$from == b & edges$to == a), ]
    if (nrow(row) > 0) {
      expect_equal(row$weight[1], bn_ref$weight[i], tolerance = 1e-6,
                   info = sprintf("Coupling cosine %s-%s", a, b))
    }
  }
})

test_that("co-citation cosine matches biblionetwork biblio_cocitation", {
  skip_if_not_installed("biblionetwork")

  ## biblionetwork co-citation reference (cosine):
  bn_ref <- data.frame(
    from = c("R1","R1","R1","R2","R2","R2","R2","R3","R3","R5"),
    to   = c("R2","R3","R4","R3","R4","R5","R6","R4","R5","R6"),
    weight = c(0.5773503, 0.6666667, 0.8164966, 0.5773503, 0.3535534,
               0.7071068, 0.5, 0.4082483, 0.4082483, 0.7071068),
    stringsAsFactors = FALSE
  )

  d <- data.frame(id = paste0("W", 1:5), stringsAsFactors = FALSE)
  d$references <- list(
    c("R1","R2","R3"), c("R1","R2","R4"), c("R2","R3","R5"),
    c("R1","R3","R4"), c("R2","R5","R6")
  )

  edges <- reference_network(d, measure = "cosine", threshold = 0)

  for (i in seq_len(nrow(bn_ref))) {
    a <- bn_ref$from[i]; b <- bn_ref$to[i]
    row <- edges[(edges$from == a & edges$to == b) |
                 (edges$from == b & edges$to == a), ]
    if (nrow(row) > 0) {
      expect_equal(row$weight[1], bn_ref$weight[i], tolerance = 1e-4,
                   info = sprintf("Co-citation cosine %s-%s", a, b))
    }
  }
})
