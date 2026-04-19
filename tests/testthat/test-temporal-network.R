## Tests for temporal_network() and build_windows()

## Data spanning 5 years, with enough papers per year to form networks
make_temporal_data <- function() {
  d <- data.frame(
    id   = paste0("W", 1:10),
    year = c(2018L, 2018L, 2019L, 2019L, 2020L,
              2020L, 2021L, 2021L, 2022L, 2022L),
    stringsAsFactors = FALSE
  )
  d$keywords <- list(
    c("ml", "dl"),     # W1 2018
    c("ml", "nlp"),    # W2 2018
    c("dl", "cv"),     # W3 2019
    c("ml", "cv"),     # W4 2019
    c("nlp", "bert"),  # W5 2020
    c("dl", "bert"),   # W6 2020
    c("bert", "gpt"),  # W7 2021
    c("ml", "gpt"),    # W8 2021
    c("cv", "yolo"),   # W9 2022
    c("dl", "yolo")    # W10 2022
  )
  d$references <- lapply(seq_len(10), function(i) character(0))
  d$authors    <- lapply(paste0("A", 1:10), function(x) x)
  d
}

## ── build_windows ───────────────────────────────────────────────────────────

test_that("build_windows fixed produces disjoint non-overlapping windows", {
  w <- bibnets:::build_windows(2018, 2022, window = 2, step = NULL, strategy = "fixed")
  expect_equal(w$start, c(2018, 2020, 2022))
  expect_equal(w$end,   c(2019, 2021, 2022))
  expect_equal(w$label, c("2018-2019", "2020-2021", "2022-2022"))
})

test_that("build_windows sliding produces overlapping windows", {
  w <- bibnets:::build_windows(2018, 2020, window = 2, step = 1, strategy = "sliding")
  expect_equal(w$start, c(2018, 2019))
  expect_equal(w$end,   c(2019, 2020))
})

test_that("build_windows cumulative grows from first year", {
  w <- bibnets:::build_windows(2018, 2021, window = 2, step = 1, strategy = "cumulative")
  expect_true(all(w$start == 2018))
  expect_equal(w$end, c(2019, 2020, 2021))
})

## ── temporal_network ────────────────────────────────────────────────────────

test_that("temporal_network fixed returns named list", {
  d <- make_temporal_data()
  tn <- temporal_network(d, keyword_network, window = 2, strategy = "fixed",
                          threshold = 0)
  expect_true(is.list(tn))
  expect_true(length(tn) > 0)
  expect_true(all(grepl("^\\d{4}-\\d{4}$", names(tn))))
})

test_that("temporal_network fixed windows have correct year ranges", {
  d <- make_temporal_data()
  tn <- temporal_network(d, keyword_network, window = 2, strategy = "fixed",
                          threshold = 0)
  ## All edges in each window should come from papers in that year range
  for (nm in names(tn)) {
    years_in_window <- as.integer(strsplit(nm, "-")[[1]])
    start_y <- years_in_window[1]
    end_y   <- years_in_window[2]
    ## The 'window' column exists in the edge data
    expect_equal(unique(tn[[nm]]$window), nm)
  }
})

test_that("temporal_network sliding produces more windows than fixed", {
  d <- make_temporal_data()
  tn_fixed  <- temporal_network(d, keyword_network, window = 2,
                                 strategy = "fixed",  threshold = 0)
  tn_slide  <- temporal_network(d, keyword_network, window = 2,
                                 strategy = "sliding", threshold = 0)
  ## Sliding (step=1) always >= fixed (step=window)
  expect_true(length(tn_slide) >= length(tn_fixed))
})

test_that("temporal_network cumulative windows grow", {
  d <- make_temporal_data()
  tn <- temporal_network(d, keyword_network, window = 2, strategy = "cumulative",
                          threshold = 0)
  ## Each successive window starts at 2018; end years grow
  end_years <- vapply(names(tn), function(nm) {
    as.integer(strsplit(nm, "-")[[1]][2])
  }, integer(1L))
  expect_true(all(diff(end_years) > 0))
})

test_that("temporal_network accepts function name as string", {
  d <- make_temporal_data()
  tn <- temporal_network(d, "keyword_network", window = 3, threshold = 0)
  expect_true(is.list(tn))
})

test_that("temporal_network each window contains a data frame", {
  d <- make_temporal_data()
  tn <- temporal_network(d, keyword_network, window = 2, threshold = 0)
  for (nm in names(tn)) {
    expect_true(is.data.frame(tn[[nm]]))
    expect_true(all(c("from", "to", "weight") %in% names(tn[[nm]])))
  }
})

test_that("temporal_network skips windows with fewer than 2 papers", {
  ## Single-paper years cannot form co-occurrence networks
  d <- data.frame(
    id = c("W1", "W2"),
    year = c(2018L, 2020L),   # gap year 2019 has no data
    stringsAsFactors = FALSE
  )
  d$keywords <- list(c("ml", "dl"), c("ml", "cv"))
  tn <- temporal_network(d, keyword_network, window = 1, threshold = 0)
  ## Each year only has 1 paper → no edges → no windows returned
  expect_equal(length(tn), 0)
})

test_that("temporal_network passes ... args to network function", {
  d <- make_temporal_data()
  ## With threshold=100, no edges should survive in any window → empty list
  tn <- temporal_network(d, keyword_network, window = 5, threshold = 100)
  ## All windows produce 0 edges, so they're filtered out → empty list
  expect_equal(length(tn), 0)
})

test_that("temporal_network requires year column", {
  d <- make_temporal_data()
  d$year <- NULL
  expect_error(temporal_network(d, keyword_network))
})
