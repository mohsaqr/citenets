#' Read an RIS file
#'
#' Parses a `.ris` file into a standardized bibliometric data frame.
#' Like BibTeX, standard RIS does not include cited references.
#'
#' @param file Path to a `.ris` file.
#'
#' @return A data frame with the same column structure as [read_scopus()].
#'
#' @export
#' @examples
#' \dontrun{
#' data <- read_ris("export.ris")
#' }
read_ris <- function(file) {
  stopifnot(file.exists(file))

  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)

  records <- list()
  current <- list()

  for (line in lines) {
    line <- trimws(line)
    if (nchar(line) == 0) next

    ## End of record
    if (grepl("^ER\\s*-", line)) {
      if (length(current) > 0) {
        records <- c(records, list(current))
      }
      current <- list()
      next
    }

    ## Tag line: "TY  - JOUR"
    if (grepl("^[A-Z][A-Z0-9]\\s+-\\s+", line)) {
      tag <- sub("\\s+-.*$", "", line)
      tag <- trimws(tag)
      value <- sub("^[A-Z][A-Z0-9]\\s+-\\s+", "", line)
      value <- trimws(value)
      current[[tag]] <- c(current[[tag]], value)
    }
  }

  if (length(current) > 0) {
    records <- c(records, list(current))
  }

  n <- length(records)
  if (n == 0) return(empty_biblio_df())

  get_ris <- function(rec, tags, collapse = NULL) {
    for (tag in tags) {
      val <- rec[[tag]]
      if (!is.null(val)) {
        if (!is.null(collapse)) return(paste(val, collapse = collapse))
        return(val[1])
      }
    }
    NA_character_
  }

  result <- data.frame(
    id = vapply(records, function(r) {
      id_val <- get_ris(r, c("DO", "AN", "ID"))
      if (is.na(id_val)) paste0("RIS", which(
        vapply(records, identical, logical(1), r)
      ))
      else id_val
    }, character(1)),
    title = vapply(records, function(r) get_ris(r, c("TI", "T1", "CT")),
                   character(1)),
    year = vapply(records, function(r) {
      y <- get_ris(r, c("PY", "Y1", "DA"))
      if (is.na(y)) return(NA_integer_)
      ## Extract 4-digit year from date strings
      m <- regmatches(y, regexpr("\\d{4}", y))
      if (length(m) > 0) as.integer(m[1]) else NA_integer_
    }, integer(1)),
    journal = vapply(records, function(r) {
      get_ris(r, c("JO", "JF", "T2", "JA"))
    }, character(1)),
    doi = vapply(records, function(r) get_ris(r, "DO"), character(1)),
    cited_by_count = NA_integer_,
    abstract = vapply(records, function(r) get_ris(r, c("AB", "N2")),
                      character(1)),
    type = vapply(records, function(r) get_ris(r, "TY"), character(1)),
    stringsAsFactors = FALSE
  )

  ## Authors (AU tag, repeatable)
  result$authors <- lapply(records, function(r) {
    au <- r[["AU"]]
    if (is.null(au)) au <- r[["A1"]]
    if (is.null(au)) return(character(0))
    standardize_authors(au)
  })

  ## Keywords (KW tag, repeatable)
  result$keywords <- lapply(records, function(r) {
    kw <- r[["KW"]]
    if (is.null(kw)) return(character(0))
    trimws(kw)
  })

  ## No references in standard RIS
  result$references <- replicate(n, character(0), simplify = FALSE)

  result
}
