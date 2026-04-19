## Cross-validation: bibnets vs bibliometrix
## Verifies numerical equivalence of network construction and normalization

test_that("co-authorship matches bibliometrix (full counting)", {
  skip_if_not_installed("bibliometrix")

  ## bibliometrix format
  M <- data.frame(
    AU = c("SMITH J;JONES A;LEE K",
           "SMITH J;BROWN M",
           "JONES A;LEE K;CHEN W",
           "BROWN M;DAVIS R;SMITH J",
           "LEE K;CHEN W"),
    DB = rep("ISI", 5),
    stringsAsFactors = FALSE
  )

  ## bibnets format
  d <- data.frame(id = paste0("W", 1:5), stringsAsFactors = FALSE)
  d$authors <- list(
    c("SMITH J", "JONES A", "LEE K"),
    c("SMITH J", "BROWN M"),
    c("JONES A", "LEE K", "CHEN W"),
    c("BROWN M", "DAVIS R", "SMITH J"),
    c("LEE K", "CHEN W")
  )

  ## bibliometrix
  A_bm <- suppressMessages(
    bibliometrix::biblioNetwork(M, analysis = "collaboration",
                                 network = "authors")
  )
  A_bm <- as.matrix(A_bm)
  diag(A_bm) <- 0

  ## bibnets
  edges_cn <- author_network(d, "collaboration", counting = "full")
  A_cn <- as.matrix(to_matrix(edges_cn))

  ## Align row/column order
  common <- sort(intersect(rownames(A_bm), rownames(A_cn)))
  A_bm <- A_bm[common, common]
  A_cn <- A_cn[common, common]

  expect_equal(A_cn, A_bm, tolerance = 1e-10,
               info = "Co-authorship full counting: bibnets vs bibliometrix")
})

test_that("co-citation via cocMatrix matches bibliometrix", {
  skip_if_not_installed("bibliometrix")

  ## Use WoS-style reference strings that bibliometrix can parse
  M <- data.frame(
    AU = c("A1;A2", "A3;A4", "A5;A6", "A7;A8", "A9;A10"),
    CR = c("SMITH 2010 NATURE;JONES 2011 SCIENCE;LEE 2012 CELL",
           "SMITH 2010 NATURE;JONES 2011 SCIENCE;CHEN 2013 JACS",
           "JONES 2011 SCIENCE;LEE 2012 CELL;BROWN 2014 PNAS",
           "SMITH 2010 NATURE;LEE 2012 CELL;CHEN 2013 JACS",
           "JONES 2011 SCIENCE;BROWN 2014 PNAS;DAVIS 2015 JBC"),
    DB = rep("ISI", 5),
    stringsAsFactors = FALSE
  )

  ## bibliometrix: build bipartite, then crossprod for co-citation
  bip_bm <- suppressMessages(bibliometrix::cocMatrix(M, Field = "CR"))
  A_bm <- Matrix::crossprod(bip_bm)
  A_bm <- as.matrix(A_bm)
  diag(A_bm) <- 0

  ## bibnets: use same ref names as bibliometrix parsed them
  ## Verified mapping: Paper 1 = JONES, LEE, SMITH; Paper 2 = JONES, SMITH, CHEN; etc.
  d <- data.frame(id = paste0("W", 1:5), stringsAsFactors = FALSE)
  d$references <- list(
    c("JONES 2011 SCIENCE", "LEE 2012 CELL", "SMITH 2010 NATURE"),
    c("JONES 2011 SCIENCE", "SMITH 2010 NATURE", "CHEN 2013 JACS"),
    c("JONES 2011 SCIENCE", "LEE 2012 CELL", "BROWN 2014 PNAS"),
    c("LEE 2012 CELL", "SMITH 2010 NATURE", "CHEN 2013 JACS"),
    c("JONES 2011 SCIENCE", "BROWN 2014 PNAS", "DAVIS 2015 JBC")
  )
  edges_cn <- reference_network(d, threshold = 0)
  A_cn <- as.matrix(to_matrix(edges_cn))

  ## Align names
  common <- sort(intersect(rownames(A_bm), rownames(A_cn)))
  A_bm <- A_bm[common, common]
  A_cn <- A_cn[common, common]

  expect_equal(A_cn, A_bm, tolerance = 1e-10,
               info = "Co-citation: bibnets vs bibliometrix cocMatrix")
})

test_that("bibliographic coupling via cocMatrix matches bibliometrix", {
  skip_if_not_installed("bibliometrix")

  M <- data.frame(
    AU = c("A1", "A2", "A3", "A4", "A5"),
    CR = c("SMITH 2010 NATURE;JONES 2011 SCIENCE;LEE 2012 CELL",
           "SMITH 2010 NATURE;JONES 2011 SCIENCE;CHEN 2013 JACS",
           "JONES 2011 SCIENCE;LEE 2012 CELL;BROWN 2014 PNAS",
           "SMITH 2010 NATURE;LEE 2012 CELL;CHEN 2013 JACS",
           "JONES 2011 SCIENCE;BROWN 2014 PNAS;DAVIS 2015 JBC"),
    DB = rep("ISI", 5),
    stringsAsFactors = FALSE
  )

  ## bibliometrix: tcrossprod for coupling
  bip_bm <- suppressMessages(bibliometrix::cocMatrix(M, Field = "CR"))
  A_bm <- Matrix::tcrossprod(bip_bm)
  A_bm <- as.matrix(A_bm)
  diag(A_bm) <- 0

  ## bibnets: use same ref names as bibliometrix parsed them
  d <- data.frame(id = paste0("W", 1:5), stringsAsFactors = FALSE)
  d$references <- list(
    c("JONES 2011 SCIENCE", "LEE 2012 CELL", "SMITH 2010 NATURE"),
    c("JONES 2011 SCIENCE", "SMITH 2010 NATURE", "CHEN 2013 JACS"),
    c("JONES 2011 SCIENCE", "LEE 2012 CELL", "BROWN 2014 PNAS"),
    c("LEE 2012 CELL", "SMITH 2010 NATURE", "CHEN 2013 JACS"),
    c("JONES 2011 SCIENCE", "BROWN 2014 PNAS", "DAVIS 2015 JBC")
  )
  edges_cn <- document_network(d, "coupling")
  A_cn <- as.matrix(to_matrix(edges_cn))

  ## Compare spectra (name-independent)
  eig_bm <- sort(eigen(A_bm, symmetric = TRUE, only.values = TRUE)$values)
  eig_cn <- sort(eigen(A_cn, symmetric = TRUE, only.values = TRUE)$values)

  expect_equal(eig_cn, eig_bm, tolerance = 1e-8,
               info = "Coupling spectrum: bibnets vs bibliometrix")
})

test_that("all 5 normalizations match bibliometrix formulas", {
  skip_if_not_installed("bibliometrix")

  ## Build a known co-occurrence matrix and compare normalization
  ## Using a hand-built matrix avoids any parsing differences
  A <- matrix(
    c(4, 2, 2, 1, 2, 1,
      2, 3, 2, 2, 0, 0,
      2, 2, 3, 1, 1, 0,
      1, 2, 1, 2, 0, 0,
      2, 0, 1, 0, 2, 1,
      1, 0, 0, 0, 1, 1),
    nrow = 6, byrow = TRUE,
    dimnames = list(paste0("R", 1:6), paste0("R", 1:6))
  )

  D <- diag(A)

  norm_methods <- list(
    association = function(A, D) A / outer(D, D, "*"),
    cosine = function(A, D) A / outer(D, D, function(a, b) sqrt(a * b)),
    jaccard = function(A, D) {
      denom <- outer(D, D, "+") - A
      denom[denom == 0] <- 1
      A / denom
    },
    inclusion = function(A, D) {
      denom <- outer(D, D, pmin)
      denom[denom == 0] <- 1
      A / denom
    },
    equivalence = function(A, D) A^2 / outer(D, D, "*")
  )

  ## Also run bibliometrix normalizeSimilarity for reference
  bm_methods <- list(
    association = "association",
    cosine = "salton",
    jaccard = "jaccard",
    inclusion = "inclusion",
    equivalence = "equivalence"
  )

  for (method_name in names(norm_methods)) {
    ## bibnets
    S_cn <- as.matrix(normalize(A, method_name))

    ## Hand-computed reference
    expected <- norm_methods[[method_name]](A, D)
    expected[is.nan(expected)] <- 0
    expected[is.infinite(expected)] <- 0
    diag(expected) <- 0

    expect_equal(S_cn, expected, tolerance = 1e-10,
                 info = sprintf("Normalization formula: %s", method_name))

    ## bibliometrix
    S_bm <- as.matrix(bibliometrix::normalizeSimilarity(
      Matrix::Matrix(A, sparse = TRUE),
      type = bm_methods[[method_name]]
    ))
    S_bm[is.nan(S_bm)] <- 0
    diag(S_bm) <- 0

    expect_equal(S_cn, S_bm, tolerance = 1e-10,
                 info = sprintf("vs bibliometrix: %s", method_name))
  }
})
