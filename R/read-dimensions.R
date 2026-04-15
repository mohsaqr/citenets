#' Read Dimensions CSV export
#'
#' Parses a CSV file exported from Dimensions into a standardized
#' bibliometric data frame.
#'
#' @param file Path to a Dimensions CSV export file.
#' @param encoding Character. File encoding. Default `"UTF-8"`.
#'
#' @return A data frame with the same column structure as [read_scopus()].
#'
#' @export
#' @examples
#' \dontrun{
#' data <- read_dimensions("dimensions_export.csv")
#' }
read_dimensions <- function(file, encoding = "UTF-8") {
  stopifnot(file.exists(file))

  raw <- utils::read.csv(file, stringsAsFactors = FALSE, fileEncoding = encoding,
                          check.names = FALSE)

  col_map <- list(
    id       = c("Publication ID", "Dimensions URL"),
    title    = c("Title"),
    year     = c("PubYear", "Publication Year", "Year"),
    journal  = c("Source title", "Source Title",
                  "Source title/Anthology title"),
    doi      = c("DOI"),
    cited_by = c("Times cited", "Times Cited", "Citation Count"),
    abstract = c("Abstract"),
    type     = c("Publication Type", "Document Type"),
    authors  = c("Authors"),
    refs     = c("Cited references", "Cited References", "References"),
    affiliations = c("Authors Affiliations Name of Research organization",
                      "Research Organizations - standardized"),
    countries = c("Authors Affiliations Country of Research organization",
                   "Countries of Research organization")
  )

  lower_names <- tolower(names(raw))
  find_col <- function(candidates) {
    idx <- match(tolower(candidates), lower_names)
    idx <- idx[!is.na(idx)]
    if (length(idx) == 0L) return(NA_character_)
    names(raw)[idx[1L]]
  }

  get_col <- function(candidates, default = NA_character_) {
    col_name <- find_col(candidates)
    if (is.na(col_name)) return(rep(default, nrow(raw)))
    raw[[col_name]]
  }

  ## Build ID
  id_raw <- get_col(col_map$id, NA_character_)
  id <- ifelse(is.na(id_raw) | nchar(id_raw) == 0,
               paste0("DIM", seq_len(nrow(raw))),
               id_raw)

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

  ## Authors: semicolon-delimited
  result$authors <- lapply(
    split_field(get_col(col_map$authors), sep = ";"),
    standardize_authors
  )

  ## References: Dimensions uses semicolons before brackets or just semicolons
  refs_raw <- get_col(col_map$refs)
  result$references <- lapply(refs_raw, function(r) {
    if (is.na(r) || nchar(trimws(r)) == 0) return(character(0))
    r <- gsub("\\[([^]]+)\\]", "\\1", r)
    standardize_refs(strsplit(r, ";")[[1]])
  })

  ## Keywords: Dimensions may have "Fields of Study" or "Research Categories"
  kw_raw <- get_col(c("Fields of Study", "Keywords", "RCDC Categories",
                        "Research Categories"))
  result$keywords <- split_field(kw_raw, sep = ";")

  ## Source-specific extras
  result$affiliations <- get_col(col_map$affiliations)
  result$countries <- split_field(get_col(col_map$countries), sep = ";")

  result
}
