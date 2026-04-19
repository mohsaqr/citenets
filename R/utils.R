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
    parts <- trimws(strsplit(s, sep, fixed = TRUE)[[1]])
    parts[nchar(parts) > 0]
  })
}


#' Standardize author names
#'
#' Uppercase, whitespace normalisation, and dot removal from initials
#' (`F.J.` → `FJ`). Name order and format are preserved — consistent with
#' how bibliometrix handles multi-source data.
#'
#' @param x Character vector of author names.
#' @param flip_names Logical. If `TRUE`, names in `Last, First` format are
#'   reordered to `First Last`. Off by default — enable only when all names
#'   in `x` reliably follow the `Last, First` convention.
#' @return Character vector, uppercased and cleaned.
#' @keywords internal
standardize_authors <- function(x, flip_names = FALSE) {
  x <- trimws(gsub("\\s+", " ", x))
  x <- x[nchar(x) > 0]
  if (length(x) == 0) return(character(0))
  x <- gsub(".", "", x, fixed = TRUE)
  if (flip_names) {
    comma <- grepl(",", x, fixed = TRUE)
    x[comma] <- vapply(strsplit(x[comma], ",\\s*"), function(parts) {
      paste(rev(trimws(parts)), collapse = " ")
    }, character(1))
  }
  toupper(x)
}


## ── Input validation helpers ────────────────────────────────────────────────

#' @keywords internal
check_data <- function(data, required) {
  if (!is.data.frame(data))
    stop("'data' must be a data frame, not ", class(data)[1], call. = FALSE)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0)
    stop("Required column(s) not found in data: ",
         paste0("'", missing, "'", collapse = ", "), call. = FALSE)
}

#' @keywords internal
check_edges <- function(edges) {
  if (!is.data.frame(edges))
    stop("'edges' must be a data frame, not ", class(edges)[1], call. = FALSE)
  missing <- setdiff(c("from", "to", "weight"), names(edges))
  if (length(missing) > 0)
    stop("Required column(s) not found in edges: ",
         paste0("'", missing, "'", collapse = ", "),
         "\nExpected columns: from, to, weight", call. = FALSE)
}

#' @keywords internal
check_choice <- function(value, choices, name) {
  if (!value %in% choices)
    stop(sprintf("'%s' is not a valid %s. Choose one of: %s",
                 value, name, paste0("'", choices, "'", collapse = ", ")),
         call. = FALSE)
}

#' @keywords internal
check_format <- function(format) {
  valid <- c("edgelist", "gephi", "igraph", "cograph", "matrix")
  check_choice(format, valid, "format")
}

#' @keywords internal
check_file <- function(file) {
  if (!file.exists(file))
    stop("File not found: ", file, call. = FALSE)
}


`%||%` <- function(a, b) if (!is.null(a)) a else b

#' @keywords internal
standardize_refs <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  ## Remove trailing DOI if present (for WoS CR field)
  x <- sub(",\\s*DOI\\s+.*$", "", x, ignore.case = TRUE)
  toupper(x)
}
