#' Convert OpenAlex data to citenets format
#'
#' Takes the output of [openalexR::oa_fetch()] (a tibble/data frame of works)
#' and converts it to the standardized citenets format with list-columns.
#'
#' @param data A data frame from `oa_fetch(entity = "works", ...)`. Must
#'   contain at least an `id` column. Common columns include `display_name`,
#'   `publication_year`, `so`, `doi`, `cited_by_count`, `referenced_works`,
#'   `ab`, and `author` (nested).
#'
#' @return A data frame with the same column structure as [read_scopus()].
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
