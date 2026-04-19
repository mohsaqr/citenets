#' Convert OpenAlex data to bibnets format
#'
#' Takes the output of [openalexR::oa_fetch()] (a tibble/data frame of works)
#' and converts it to the standardized bibnets format with list-columns.
#'
#' @param data A data frame from `oa_fetch(entity = "works", ...)`. Must
#'   contain at least an `id` column. Common columns include `display_name`,
#'   `publication_year`, `so`, `doi`, `cited_by_count`, `referenced_works`,
#'   `ab`, and `author` (nested).
#'
#' @return A data frame in the standard bibnets format: `id`, `title`,
#'   `year`, `journal`, `doi`, `cited_by_count`, `abstract`, `type`,
#'   plus list-columns `authors`, `references`, and `keywords`.
#'
#' @export
#' @examples
#' \dontrun{
#' library(openalexR)
#' raw <- oa_fetch(entity = "works", search = "bibliometric networks",
#'                 count_only = FALSE)
#' data <- read_openalex(raw)
#' }
read_openalex <- function(data) {
  stopifnot(is.data.frame(data))

  n <- nrow(data)

  ## Helper: safely get column
  safe_col <- function(name, default = NA_character_) {
    if (name %in% names(data)) data[[name]]
    else rep(default, n)
  }

  ## ID: OpenAlex work ID
  id <- safe_col("id", paste0("OA", seq_len(n)))

  ## Title
  title <- safe_col("display_name")

  ## Year
  year <- as.integer(safe_col("publication_year", NA_integer_))

  ## Journal
  journal <- safe_col("so")

  ## DOI
  doi <- safe_col("doi")
  ## OpenAlex prefixes DOIs with "https://doi.org/"
  doi <- sub("^https://doi\\.org/", "", doi)

  ## Cited by count
  cited_by <- as.integer(safe_col("cited_by_count", 0L))

  ## Abstract
  abstract <- safe_col("ab")

  ## Type
  type <- safe_col("type")

  ## Authors: openalexR stores in nested `author` column
  if ("author" %in% names(data) && is.list(data[["author"]])) {
    authors <- lapply(data[["author"]], function(au) {
      if (is.null(au) || !is.data.frame(au)) return(character(0))
      name_col <- intersect(c("au_display_name", "display_name", "au_name"),
                             names(au))
      if (length(name_col) == 0) return(character(0))
      standardize_authors(au[[name_col[1]]])
    })
  } else {
    authors <- replicate(n, character(0), simplify = FALSE)
  }

  ## References: openalexR stores as `referenced_works` (character vector of IDs)
  if ("referenced_works" %in% names(data)) {
    references <- if (is.list(data[["referenced_works"]])) {
      lapply(data[["referenced_works"]], function(r) {
        if (is.null(r)) character(0) else as.character(r)
      })
    } else {
      split_field(as.character(data[["referenced_works"]]), sep = ",")
    }
  } else {
    references <- replicate(n, character(0), simplify = FALSE)
  }

  ## Keywords: openalexR may have `concepts` or `keywords` column
  keywords <- if ("concepts" %in% names(data) && is.list(data[["concepts"]])) {
    lapply(data[["concepts"]], function(c) {
      if (is.null(c) || !is.data.frame(c)) return(character(0))
      name_col <- intersect(c("display_name", "concept_name"), names(c))
      if (length(name_col) == 0) return(character(0))
      as.character(c[[name_col[1]]])
    })
  } else if ("keywords" %in% names(data) && is.list(data[["keywords"]])) {
    lapply(data[["keywords"]], function(k) {
      if (is.null(k)) return(character(0))
      if (is.data.frame(k)) {
        name_col <- intersect(c("display_name", "keyword"), names(k))
        if (length(name_col) > 0) as.character(k[[name_col[1]]])
        else character(0)
      } else {
        as.character(k)
      }
    })
  } else {
    replicate(n, character(0), simplify = FALSE)
  }

  result <- data.frame(
    id = id,
    title = title,
    year = year,
    journal = journal,
    doi = doi,
    cited_by_count = cited_by,
    abstract = abstract,
    type = type,
    stringsAsFactors = FALSE
  )

  result$authors <- authors
  result$references <- references
  result$keywords <- keywords

  result
}


#' Read a flat OpenAlex CSV export
#'
#' Reads the flat CSV format downloaded directly from the OpenAlex website
#' (`openalex.org/works` exports). Multi-value fields are pipe-delimited (`|`).
#' This is distinct from the nested tibble produced by [openalexR::oa_fetch()],
#' which is handled by [read_openalex()].
#'
#' @param file Path to the CSV file.
#' @param sep Character. Delimiter for multi-value fields. Default `"|"`.
#'
#' @return A data frame in the standard bibnets format: `id`, `title`,
#'   `year`, `journal`, `doi`, `cited_by_count`, `abstract`, `type`,
#'   plus list-columns `authors`, `references`, `keywords`, `affiliations`,
#'   `countries`. `abstract` and `references` are always `NA` / empty
#'   (not available in the flat export).
#'
#' @export
#' @examples
#' f <- system.file("extdata", "openalex_works.csv", package = "bibnets")
#' data <- read_openalex_csv(f)
read_openalex_csv <- function(file, sep = "|") {
  check_file(file)

  raw <- utils::read.csv(file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                          check.names = FALSE)

  n <- nrow(raw)

  safe_col <- function(col, default = NA_character_) {
    if (col %in% names(raw)) as.character(raw[[col]])
    else rep(default, n)
  }

  id          <- sub("^https://openalex\\.org/", "", safe_col("id"))
  title       <- safe_col("display_name")
  year        <- suppressWarnings(as.integer(safe_col("publication_year")))
  journal     <- safe_col("primary_location.source.display_name")
  doi         <- sub("^https://doi\\.org/", "", safe_col("doi"))
  doi[doi == ""] <- NA_character_
  cited_by    <- suppressWarnings(as.integer(safe_col("cited_by_count")))
  cited_by[is.na(cited_by)] <- 0L
  abstract    <- rep(NA_character_, n)
  type        <- safe_col("type")
  authors     <- split_field(safe_col("authorships.author.display_name", ""), sep = sep)
  affiliations <- split_field(safe_col("authorships.institutions.display_name", ""), sep = sep)
  countries   <- split_field(safe_col("authorships.countries", ""), sep = sep)
  keywords    <- split_field(safe_col("primary_topic.display_name", ""), sep = sep)
  references  <- vector("list", n)

  result <- data.frame(
    id            = id,
    title         = title,
    year          = year,
    journal       = journal,
    doi           = doi,
    cited_by_count = cited_by,
    abstract      = abstract,
    type          = type,
    stringsAsFactors = FALSE
  )

  result$authors      <- authors
  result$references   <- references
  result$keywords     <- keywords
  result$affiliations <- affiliations
  result$countries    <- countries

  result
}
