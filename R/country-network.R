#' Build a country network
#'
#' Constructs a network between countries based on collaboration or coupling.
#'
#' @param data A data frame with `id` and `countries` (list-column of
#'   country names). For coupling, also needs `references`.
#' @param type Character. `"collaboration"` (default), `"coupling"`, or
#'   `"equivalence"`.
#' @param counting Character. Counting method. Default `"full"`.
#' @param similarity Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum papers per country. Default 1.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @export
#' @examples
#' \dontrun{
#' country_network(data, "collaboration")
#' }
country_network <- function(data,
                            type = "collaboration",
                            counting = "full",
                            similarity = "none",
                            threshold = 0,
                            min_occur = 1L) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "countries" %in% names(data),
    type %in% c("collaboration", "coupling", "equivalence"),
    counting %in% position_independent_counts(),
    similarity %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  if (type == "collaboration") {
    B <- build_bipartite(data, field = "countries", min_freq = min_occur)
    B <- apply_counting(B, counting = counting, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", similarity = similarity,
                       threshold = threshold)

  } else if (type == "coupling") {
    stopifnot("references" %in% names(data))
    agg <- aggregate_by_entity(data, entity_field = "countries",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    B <- apply_counting(B, counting = counting, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", similarity = similarity,
                       threshold = threshold)

  } else if (type == "equivalence") {
    B <- build_bipartite(data, field = "countries", min_freq = min_occur)
    multiply_bipartite(B, mode = "columns", similarity = "cosine",
                       threshold = threshold)
  }
}
