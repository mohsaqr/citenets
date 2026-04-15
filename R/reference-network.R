#' Build a reference network
#'
#' Constructs a co-citation or equivalence network among cited references.
#' Two references are linked when they are cited together by the same paper.
#'
#' @param data A data frame with `id` and `references` (list-column).
#' @param type Character. `"co_citation"` (default) or `"equivalence"`.
#' @param counting Character. Counting method. Default `"full"`.
#' @param similarity Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 1.
#' @param min_occur Integer. Minimum times a reference must be cited. Default 1.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @references
#' Small, H. (1973). Co-citation in the scientific literature.
#' *JASIS*, 24(4), 265--269.
#'
#' @export
#' @examples
#' data(biblio_data)
#' reference_network(biblio_data)
#' reference_network(biblio_data, similarity = "association")
reference_network <- function(data,
                              type = "co_citation",
                              counting = "full",
                              similarity = "none",
                              threshold = 1,
                              min_occur = 1L) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "references" %in% names(data),
    type %in% c("co_citation", "equivalence"),
    counting %in% position_independent_counts(),
    similarity %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  B <- build_bipartite(data, field = "references", min_freq = min_occur)

  if (type == "equivalence") {
    multiply_bipartite(B, mode = "columns", similarity = "cosine",
                       threshold = threshold)
  } else {
    B <- apply_counting(B, counting = counting, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", similarity = similarity,
                       threshold = threshold)
  }
}
