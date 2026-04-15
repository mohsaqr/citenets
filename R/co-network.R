#' Build a co-occurrence network from any field
#'
#' With one field, entities are linked when they co-occur in the same
#' document. With `by`, entities are linked when they share values of the
#' `by` field across documents.
#'
#' Fields can be list-columns (already split) or character columns with
#' delimiters (auto-split via `sep`).
#'
#' @param data A data frame with column `id` and the specified field(s).
#' @param field Character. The entity field — determines what the nodes are.
#' @param by Character or `NULL`. What links the nodes. If `NULL` (default),
#'   entities are linked by co-occurring in the same document. If specified,
#'   entities are linked when they share values from the `by` field.
#' @param sep Character or `NULL`. Delimiter for splitting character columns.
#'   Default `";"`. Set to `NULL` if columns are already list-columns.
#' @param count Character. Counting method. Default `"full"`.
#' @param measure Character. Normalization method. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum entity frequency. Default 1.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @export
#' @examples
#' data(biblio_data)
#'
#' # Co-occurrence: keywords appearing in the same document
#' co_network(biblio_data, "keywords")
#'
#' # Authors linked by shared keywords
#' co_network(biblio_data, "authors", by = "keywords")
#'
#' # Keywords linked by shared authors
#' co_network(biblio_data, "keywords", by = "authors")
#'
#' # Journals linked by shared references (= journal coupling)
#' co_network(biblio_data, "journal", by = "references", measure = "cosine")
#'
#' # Auto-splits semicolon-delimited string columns
#' d <- data.frame(id = 1:3, tags = c("ml; dl; nlp", "ml; cv", "dl; cv"))
#' co_network(d, "tags")
co_network <- function(data,
                       field,
                       by = NULL,
                       sep = ";",
                       count = "full",
                       measure = "none",
                       threshold = 0,
                       min_occur = 1L) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    field %in% names(data),
    count %in% position_independent_counts(),
    measure %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  data <- ensure_list_column(data, field, sep)

  if (is.null(by)) {
    ## Co-occurrence within one field (same document)
    B <- build_bipartite(data, field = field, min_freq = min_occur)
    B <- apply_counting(B, count = count, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", measure = measure,
                       threshold = threshold)
  } else {
    ## Entities linked by shared values from `by` field
    stopifnot(by %in% names(data))
    data <- ensure_list_column(data, by, sep)
    build_by_network(data, field = field, by = by,
                     count = count, measure = measure,
                     threshold = threshold, min_occur = min_occur)
  }
}


#' Build a network where entities share values from another field
#' @keywords internal
build_by_network <- function(data, field, by, count, measure,
                              threshold, min_occur) {
  field_col <- data[[field]]
  by_col <- data[[by]]

  ## Expand field → (doc_idx, entity)
  if (is.list(field_col)) {
    f_doc <- rep(seq_len(nrow(data)), lengths(field_col))
    f_val <- unlist(field_col, use.names = FALSE)
  } else {
    f_doc <- seq_len(nrow(data))
    f_val <- as.character(field_col)
  }

  ## Expand by → (doc_idx, by_value)
  if (is.list(by_col)) {
    b_doc <- rep(seq_len(nrow(data)), lengths(by_col))
    b_val <- unlist(by_col, use.names = FALSE)
  } else {
    b_doc <- seq_len(nrow(data))
    b_val <- as.character(by_col)
  }

  ## Clean
  keep_f <- !is.na(f_val) & nchar(f_val) > 0
  f_doc <- f_doc[keep_f]; f_val <- f_val[keep_f]
  keep_b <- !is.na(b_val) & nchar(b_val) > 0
  b_doc <- b_doc[keep_b]; b_val <- b_val[keep_b]

  ## Frequency filter on field entities
  if (min_occur > 1L) {
    freq <- table(f_val)
    keep <- f_val %in% names(freq[freq >= min_occur])
    f_doc <- f_doc[keep]; f_val <- f_val[keep]
  }

  ## Join: for each entity, collect all by-values through shared docs
  f_df <- data.frame(doc = f_doc, entity = f_val, stringsAsFactors = FALSE)
  b_df <- data.frame(doc = b_doc, by_val = b_val, stringsAsFactors = FALSE)
  pairs <- merge(f_df, b_df, by = "doc")

  if (nrow(pairs) == 0L) {
    return(data.frame(from = character(0), to = character(0),
                      weight = numeric(0), count = integer(0),
                      shared = integer(0), stringsAsFactors = FALSE))
  }

  ## Unique entity × by_value pairs
  pairs <- unique(pairs[, c("entity", "by_val")])

  ## Aggregate per entity: which by-values does each entity have?
  agg <- stats::aggregate(by_val ~ entity, data = pairs,
                           FUN = function(x) list(unique(x)))
  agg_df <- data.frame(id = agg$entity, stringsAsFactors = FALSE)
  agg_df[["values"]] <- agg$by_val

  ## Build bipartite: entities × by_values, then project to entity × entity
  B <- build_bipartite(agg_df, field = "values", min_freq = 1L)
  B <- apply_counting(B, count = count, network_type = "symmetric")
  multiply_bipartite(B, mode = "rows", measure = measure,
                     threshold = threshold)
}


#' Ensure a column is a list-column, splitting if needed
#' @keywords internal
ensure_list_column <- function(data, field, sep = ";") {
  col <- data[[field]]
  if (!is.list(col) && !is.null(sep)) {
    data[[field]] <- split_field(col, sep = sep)
  } else if (!is.list(col)) {
    data[[field]] <- as.list(as.character(col))
  }
  data
}
