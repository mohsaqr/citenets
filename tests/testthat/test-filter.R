## Tests for filter_top()

test_that("filter_top keeps only edges among top-n nodes", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration")

  ## 4 authors: Alice(2 papers), Bob(2 papers), Carol(2 papers), Dan(1 paper)
  ## Degree in the network: Alice touches 2 edges, Bob touches 2, Carol touches 2, Dan touches 1
  top3 <- filter_top(edges, 3)

  top_nodes <- unique(c(top3$from, top3$to))
  expect_true(length(top_nodes) <= 3)
})

test_that("filter_top returns all edges when n >= number of nodes", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration")
  n_nodes <- length(unique(c(edges$from, edges$to)))

  all_edges <- filter_top(edges, n_nodes)
  expect_equal(nrow(all_edges), nrow(edges))
})

test_that("filter_top returns empty when n=0 edges result", {
  ## Single edge — 2 nodes. filter_top(n=1) can only include 1 node → no edge
  edges <- data.frame(from = "A", to = "B", weight = 1,
                       stringsAsFactors = FALSE)
  ## filter_top(1): top 1 node is A or B; the edge requires both → 0 edges
  top1 <- filter_top(edges, 1)
  expect_equal(nrow(top1), 0)
})

test_that("filter_top sorts result by weight descending", {
  d <- make_test_data()
  edges <- author_network(d, "collaboration", counting = "full")
  top <- filter_top(edges, 4)
  if (nrow(top) > 1) {
    expect_true(all(diff(top$weight) <= 0))
  }
})

test_that("filter_top validates inputs", {
  edges <- data.frame(from = "A", to = "B", weight = 1, stringsAsFactors = FALSE)
  expect_error(filter_top(edges, 0))
  expect_error(filter_top(edges, -1))
  expect_error(filter_top(list(), 2))
})

test_that("filter_top handles empty edge list", {
  empty <- data.frame(from = character(0), to = character(0),
                       weight = numeric(0), stringsAsFactors = FALSE)
  top <- filter_top(empty, 5)
  expect_equal(nrow(top), 0)
})
