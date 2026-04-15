#' Build an institution network
#'
#' Constructs a network between institutions (affiliations).
#'
#' @param data A data frame with `id` and `affiliations` (list-column of
#'   institution names). For coupling, also needs `references`.
#' @param type Character. `"collaboration"` (default), `"coupling"`, or
#'   `"equivalence"`.
#' @param count Character. Counting method. Default `"full"`.
#' @param measure Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum papers per institution. Default 1.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @export
#' @examples
#' \dontrun{
#' institution_network(data, "collaboration")
#' }
institution_network <- function(data,
                                type = "collaboration",
                                count = "full",
                                measure = "none",
                                threshold = 0,
                                min_occur = 1L) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "affiliations" %in% names(data),
    type %in% c("collaboration", "coupling", "equivalence"),
    count %in% position_independent_counts(),
    measure %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  if (type == "collaboration") {
    B <- build_bipartite(data, field = "affiliations", min_freq = min_occur)
    B <- apply_counting(B, count = count, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", measure = measure,
                       threshold = threshold)

  } else if (type == "coupling") {
    stopifnot("references" %in% names(data))
    agg <- aggregate_by_entity(data, entity_field = "affiliations",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    B <- apply_counting(B, count = count, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", measure = measure,
                       threshold = threshold)

  } else if (type == "equivalence") {
    B <- build_bipartite(data, field = "affiliations", min_freq = min_occur)
    multiply_bipartite(B, mode = "columns", measure = "cosine",
                       threshold = threshold)
  }
}
