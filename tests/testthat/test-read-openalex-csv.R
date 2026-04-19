test_that("read_openalex_csv returns standard columns", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_true(all(c("id", "title", "year", "journal", "doi",
                    "cited_by_count", "abstract", "type",
                    "authors", "references", "keywords",
                    "affiliations", "countries") %in% names(d)))
})

test_that("read_openalex_csv returns 1508 rows from bundled dataset", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_equal(nrow(d), 1508L)
})

test_that("read_openalex_csv strips OpenAlex URL prefix from id", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_false(any(grepl("https://openalex.org/", d$id)))
  expect_true(all(grepl("^W[0-9]+$", d$id)))
})

test_that("read_openalex_csv strips DOI URL prefix", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  doi_present <- d$doi[!is.na(d$doi)]
  expect_false(any(grepl("^https://doi.org/", doi_present)))
})

test_that("read_openalex_csv produces list-columns for authors, references, keywords", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_true(is.list(d$authors))
  expect_true(is.list(d$references))
  expect_true(is.list(d$keywords))
  expect_true(is.list(d$affiliations))
  expect_true(is.list(d$countries))
})

test_that("read_openalex_csv pipe-splits authors correctly", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  multi_author <- Filter(function(x) length(x) > 1, d$authors)
  expect_true(length(multi_author) > 0)
})

test_that("read_openalex_csv references column is always empty", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_true(all(vapply(d$references, length, integer(1)) == 0L))
})

test_that("read_openalex_csv abstract column is all NA", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_true(all(is.na(d$abstract)))
})

test_that("read_openalex_csv year is integer", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_type(d$year, "integer")
})

test_that("read_openalex_csv cited_by_count is integer with no NAs", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  expect_type(d$cited_by_count, "integer")
  expect_false(any(is.na(d$cited_by_count)))
})

test_that("read_biblio auto-detects openalex_csv format", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_biblio(f)
  expect_equal(nrow(d), 1508L)
  expect_true(is.list(d$authors))
})

test_that("read_biblio with format='openalex_csv' works explicitly", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_biblio(f, format = "openalex_csv")
  expect_equal(nrow(d), 1508L)
})

test_that("read_openalex_csv countries are pipe-split into character vectors", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  multi_country <- Filter(function(x) length(x) > 1, d$countries)
  expect_true(length(multi_country) > 0)
  all_codes <- unlist(d$countries)
  expect_true(all(nchar(all_codes) == 2L))
})

test_that("read_openalex_csv keywords are single-element lists from primary_topic", {
  f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
  d <- read_openalex_csv(f)
  kw_lengths <- vapply(d$keywords, length, integer(1))
  expect_true(all(kw_lengths %in% c(0L, 1L)))
  expect_true(any(kw_lengths == 1L))
})

test_that("read_openalex_csv errors on non-existent file", {
  expect_error(read_openalex_csv("no_such_file.csv"))
})
