#' Read Lens.org CSV export
#'
#' Parses a CSV file exported from Lens.org into a standardized bibliometric
#' data frame.
#'
#' @param file Path to a Lens.org CSV export file.
#'
#' @return A data frame with the same column structure as [read_scopus()].
#'
#' @export
#' @examples
#' \dontrun{
#' data <- read_lens("lens_export.csv")
#' }
read_lens <- function(file) {
  stopifnot(file.exists(file))

  raw <- utils::read.csv(file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                          check.names = FALSE)

  n <- nrow(raw)

  get_col <- function(names_vec, default = NA_character_) {
    for (nm in names_vec) {
      if (nm %in% names(raw)) return(raw[[nm]])
    }
    rep(default, n)
  }

  id <- get_col(c("Lens ID", "ID"), paste0("LENS", seq_len(n)))

  result <- data.frame(
    id = id,
    title = get_col(c("Title")),
    year = as.integer(get_col(c("Publication Year", "Year of Publication"),
                               NA_integer_)),
    journal = get_col(c("Source Title", "Journal")),
    doi = get_col(c("DOI")),
    cited_by_count = as.integer(get_col(
      c("Citing Works Count", "Cited By Count", "Times Cited"), 0L
    )),
    abstract = get_col(c("Abstract")),
    type = get_col(c("Publication Type", "Document Type")),
    stringsAsFactors = FALSE
  )

  result$authors <- split_field(
    get_col(c("Authors", "Author/s")), sep = ";"
  )
  result$authors <- lapply(result$authors, standardize_authors)

  result$references <- split_field(
    get_col(c("References", "Cited Works")), sep = ";"
  )
  result$references <- lapply(result$references, standardize_refs)

  result$keywords <- split_field(
    get_col(c("Keywords", "MeSH Terms", "Fields of Study")), sep = ";"
  )
  result$keywords <- lapply(result$keywords, trimws)

  result
}
