#' Build a keyword co-occurrence network
#'
#' Constructs a network where two keywords are linked when they appear
#' together in the same document.
#'
#' @param data A data frame with `id` and a keyword list-column.
#' @param field Character. Name of the keyword list-column. Default
#'   `"keywords"`. Alternatives: `"author_keywords"`, `"index_keywords"`,
#'   `"keywords_plus"`.
#' @param count Character. Counting method. Default `"full"`.
#' @param measure Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 1.
#' @param min_occur Integer. Minimum keyword frequency. Default 1.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @export
#' @examples
#' data(biblio_data)
#' keyword_network(biblio_data)
#' keyword_network(biblio_data, measure = "association")
keyword_network <- function(data,
                            field = "keywords",
                            count = "full",
                            measure = "none",
                            threshold = 1,
                            min_occur = 1L) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    field %in% names(data),
    is.list(data[[field]]),
    count %in% position_independent_counts(),
    measure %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  B <- build_bipartite(data, field = field, min_freq = min_occur)
  B <- apply_counting(B, count = count, network_type = "symmetric")
  multiply_bipartite(B, mode = "columns", measure = measure,
                     threshold = threshold)
}
