#' Read Scopus CSV export
#'
#' Parses a CSV file exported from Scopus into a standardized bibliometric
#' data frame with list-columns for multi-valued fields.
#'
#' @param file Path to a Scopus CSV export file.
#' @param encoding Character. File encoding. Default `"UTF-8"`.
#'
#' @return A data frame in the standard bibnets format: `id`, `title`,
#'   `year`, `journal`, `doi`, `cited_by_count`, `abstract`, `type`,
#'   plus list-columns `authors`, `references`, and `keywords`.
#'   Scopus-specific extras: `index_keywords` (list-column),
#'   `affiliations` (character), `language` (character).
#'
#' @export
#' @examples
#' \dontrun{
#' data <- read_scopus("scopus_export.csv")
#' }
read_scopus <- function(file, encoding = "UTF-8") {
  check_file(file)

  raw <- utils::read.csv(file, stringsAsFactors = FALSE, fileEncoding = encoding,
                          check.names = FALSE)

  ## Scopus column name mapping (case-insensitive matching)
  col_map <- list(
    title    = c("Title", "Document Title"),
    authors  = c("Authors", "Author full names"),
    year     = c("Year"),
    journal  = c("Source title", "Source Title"),
    doi      = c("DOI"),
    refs     = c("References"),
    keywords = c("Author Keywords"),
    index_kw = c("Index Keywords"),
    cited_by = c("Cited by"),
    abstract = c("Abstract"),
    eid      = c("EID"),
    affiliations = c("Affiliations", "Authors with affiliations"),
    type     = c("Document Type"),
    language = c("Language of Original Document")
  )

  find_col <- function(candidates) {
    for (cand in candidates) {
      idx <- match(tolower(cand), tolower(names(raw)))
      if (!is.na(idx)) return(names(raw)[idx])
    }
    NA_character_
  }

  get_col <- function(candidates, default = NA_character_) {
    col_name <- find_col(candidates)
    if (is.na(col_name)) return(rep(default, nrow(raw)))
    raw[[col_name]]
  }

  ## Build ID
  eid <- get_col(col_map$eid, NA_character_)
  id <- ifelse(is.na(eid) | nchar(eid) == 0,
               paste0("S", seq_len(nrow(raw))),
               eid)

  result <- data.frame(
    id = id,
    title = get_col(col_map$title),
    year = as.integer(get_col(col_map$year, NA_integer_)),
    journal = get_col(col_map$journal),
    doi = get_col(col_map$doi),
    cited_by_count = as.integer(get_col(col_map$cited_by, 0L)),
    abstract = get_col(col_map$abstract),
    type = get_col(col_map$type),
    stringsAsFactors = FALSE
  )

  result$authors <- split_field(get_col(col_map$authors), sep = ";")
  result$authors <- lapply(result$authors, standardize_authors)

  result$references <- split_field(get_col(col_map$refs), sep = ";")
  result$references <- lapply(result$references, standardize_refs)

  result$keywords <- split_field(get_col(col_map$keywords), sep = ";")
  result$keywords <- lapply(result$keywords, trimws)

  ## Source-specific extras
  result$index_keywords <- split_field(get_col(col_map$index_kw), sep = ";")
  result$index_keywords <- lapply(result$index_keywords, trimws)

  result$affiliations <- get_col(col_map$affiliations)
  result$language <- get_col(col_map$language)

  result
}
