#' Build a keyword co-occurrence network
#'
#' Constructs a network where two keywords are linked when they appear
#' together in the same document.
#'
#' @param data A data frame with `id` and a keyword list-column.
#' @param field Character. Name of the keyword list-column. Default
#'   `"keywords"`. Alternatives: `"author_keywords"`, `"index_keywords"`,
#'   `"keywords_plus"`.
#' @param counting Character. Counting method. Default `"full"`.
#' @param similarity Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 1.
#' @param min_occur Integer. Minimum keyword frequency. Default 1.
#' @param top_n Integer or NULL. Return only the top n edges by weight.
#'   Default NULL (all edges).
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @export
#' @examples
#' data(biblio_data)
#' keyword_network(biblio_data)
#' keyword_network(biblio_data, similarity = "association")
keyword_network <- function(data,
                            field = "keywords",
                            counting = "full",
                            similarity = "none",
                            threshold = 1,
                            min_occur = 1L,
                            top_n = NULL) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    field %in% names(data),
    is.list(data[[field]]),
    counting %in% position_independent_counts(),
    similarity %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  B <- build_bipartite(data, field = field, min_freq = min_occur)
  B <- apply_counting(B, counting = counting, network_type = "symmetric")
  result <- multiply_bipartite(B, mode = "columns", similarity = similarity,
                               threshold = threshold, top_n = top_n)
  as_citenets_network(result, network_type = "keyword_co_occurrence",
                      counting = counting, similarity = similarity)
}
