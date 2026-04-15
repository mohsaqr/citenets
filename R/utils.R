#' Aggregate multi-valued fields by an entity
#'
#' Groups documents by a single-valued or list-column entity (e.g., author,
#' journal) and pools all values from another list-column (e.g., references,
#' keywords) across documents belonging to that entity.
#'
#' @param data A data frame with `id` and the specified columns.
#' @param entity_field Character. Name of the entity column. If it is a scalar
#'   column (e.g., `"journal"`), each document belongs to one entity. If it
#'   is a list-column (e.g., `"authors"`), each document may belong to
#'   multiple entities.
#' @param value_field Character. Name of the list-column to aggregate
#'   (e.g., `"references"`).
#'
#' @return A data frame with columns `id` (entity name) and `value_field`
#'   (list-column of pooled values, with duplicates preserved).
#'
#' @keywords internal
aggregate_by_entity <- function(data, entity_field, value_field) {
  stopifnot(
    is.data.frame(data),
    entity_field %in% names(data),
    value_field %in% names(data)
  )

  entities_col <- data[[entity_field]]
  values_col <- data[[value_field]]

  if (is.list(entities_col)) {
    ## Multi-valued entity: expand
    entity_names <- unlist(entities_col, use.names = FALSE)
    doc_idx <- rep(seq_len(nrow(data)), lengths(entities_col))
  } else {
    ## Scalar entity
    entity_names <- as.character(entities_col)
    doc_idx <- seq_len(nrow(data))
  }

  ## Pool values by entity
  unique_entities <- sort(unique(entity_names))
  pooled <- lapply(unique_entities, function(e) {
    rows <- doc_idx[entity_names == e]
    if (is.list(values_col)) {
      unlist(values_col[rows], use.names = FALSE)
    } else {
      values_col[rows]
    }
  })

  result <- data.frame(id = unique_entities, stringsAsFactors = FALSE)
  result[[value_field]] <- pooled
  result
}


#' Parse semicolon-delimited strings into list-column
#'
#' Splits semicolon-separated strings (common in Scopus/WoS exports) into
#' character vectors, trimming whitespace.
#'
#' @param x A character vector of semicolon-delimited strings.
#' @param sep Character. Delimiter. Default `";"`.
#'
#' @return A list of character vectors.
#'
#' @export
#' @examples
#' split_field(c("Alice; Bob; Carol", "Dave; Eve"))
split_field <- function(x, sep = ";") {
  lapply(x, function(s) {
    if (is.na(s) || nchar(trimws(s)) == 0) return(character(0))
    trimws(strsplit(s, sep, fixed = TRUE)[[1]])
  })
}


#' Standardize author names
#'
#' Converts author names to a standardized "LASTNAME, FI" format for
#' consistent matching across records.
#'
#' @param x Character vector of author names.
#'
#' @return Character vector of standardized names.
#' @keywords internal
standardize_authors <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  toupper(x)
}


#' Standardize reference strings
#'
#' Cleans and normalizes cited reference strings for consistent matching.
#'
#' @param x Character vector of reference strings.
#'
#' @return Character vector of cleaned reference strings.
#' @keywords internal
standardize_refs <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  ## Remove trailing DOI if present (for WoS CR field)
  x <- sub(",\\s*DOI\\s+.*$", "", x, ignore.case = TRUE)
  toupper(x)
}
