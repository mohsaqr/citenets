#' Convert Crossref API data to citenets format
#'
#' Takes the output of [rcrossref::cr_works()] (the `$data` tibble/data frame)
#' and converts it to the standardized citenets format.
#'
#' @param data A data frame from `cr_works(...)$data`.
#'
#' @return A data frame with the same column structure as [read_scopus()].
#'
#' @export
#' @examples
#' \dontrun{
#' library(rcrossref)
#' raw <- cr_works(query = "bibliometric networks")
#' data <- read_crossref(raw$data)
#' }
read_crossref <- function(data) {
  stopifnot(is.data.frame(data))

  n <- nrow(data)

  safe_col <- function(name, default = NA_character_) {
    ## Crossref uses both dot and hyphen notation
    candidates <- c(name, gsub("\\.", "-", name), gsub("-", ".", name))
    for (cand in candidates) {
      if (cand %in% names(data)) return(data[[cand]])
    }
    rep(default, n)
  }

  ## ID: DOI is the primary identifier
  doi <- safe_col("doi")
  id <- ifelse(is.na(doi) | nchar(doi) == 0,
               paste0("CR", seq_len(n)),
               doi)

  ## Year: from issued field (may be nested or string)
  issued <- safe_col("issued")
  year <- vapply(issued, function(x) {
    if (is.na(x)) return(NA_integer_)
    m <- regmatches(as.character(x), regexpr("\\d{4}", as.character(x)))
    if (length(m) > 0) as.integer(m[1]) else NA_integer_
  }, integer(1))

  result <- data.frame(
    id = id,
    title = safe_col("title"),
    year = year,
    journal = safe_col("container.title"),
    doi = doi,
    cited_by_count = as.integer(safe_col("is.referenced.by.count", 0L)),
    abstract = safe_col("abstract"),
    type = safe_col("type"),
    stringsAsFactors = FALSE
  )

  ## Authors: nested data.frame with 'given' and 'family'
  if ("author" %in% names(data) && is.list(data[["author"]])) {
    result$authors <- lapply(data[["author"]], function(au) {
      if (is.null(au) || !is.data.frame(au)) return(character(0))
      family <- if ("family" %in% names(au)) au$family else character(0)
      given <- if ("given" %in% names(au)) au$given else ""
      ## Format as "FAMILY GIVEN"
      standardize_authors(paste(trimws(family), trimws(given)))
    })
  } else {
    result$authors <- replicate(n, character(0), simplify = FALSE)
  }

  ## References: nested data.frame with DOI, article-title, author, year etc.
  if ("reference" %in% names(data) && is.list(data[["reference"]])) {
    result$references <- lapply(data[["reference"]], function(ref) {
      if (is.null(ref) || !is.data.frame(ref)) return(character(0))
      ## Prefer DOI as identifier, fall back to constructed string
      if ("DOI" %in% names(ref)) {
        doi_refs <- ref$DOI
        ## For refs without DOI, construct a string
        no_doi <- is.na(doi_refs) | nchar(doi_refs) == 0
        if (any(no_doi)) {
          au <- if ("author" %in% names(ref)) ref$author[no_doi] else ""
          yr <- if ("year" %in% names(ref)) ref$year[no_doi] else ""
          jt <- if ("journal-title" %in% names(ref)) {
            ref[["journal-title"]][no_doi]
          } else ""
          doi_refs[no_doi] <- paste(au, yr, jt)
        }
        standardize_refs(doi_refs)
      } else {
        ## No DOI column — use unstructured or construct
        if ("unstructured" %in% names(ref)) {
          standardize_refs(ref$unstructured)
        } else {
          au <- if ("author" %in% names(ref)) ref$author else ""
          yr <- if ("year" %in% names(ref)) ref$year else ""
          standardize_refs(paste(au, yr))
        }
      }
    })
  } else {
    result$references <- replicate(n, character(0), simplify = FALSE)
  }

  ## Keywords: Crossref has 'subject' field
  if ("subject" %in% names(data) && is.list(data[["subject"]])) {
    result$keywords <- lapply(data[["subject"]], function(s) {
      if (is.null(s)) character(0) else as.character(s)
    })
  } else {
    result$keywords <- replicate(n, character(0), simplify = FALSE)
  }

  result
}
