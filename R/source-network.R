#' Build a source (journal) network
#'
#' Constructs a network between publication sources (journals, book series).
#'
#' @param data A data frame with `id` and `journal` (character column).
#'   For coupling, also needs `references`. For co-citation, needs a
#'   `cited_journals` list-column.
#' @param type Character. `"coupling"` (default), `"co_citation"`, or
#'   `"equivalence"`.
#' @param counting Character. Counting method. Default `"full"`.
#' @param similarity Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum papers per source. Default 1.
#' @param top_n Integer or NULL. Return only the top n edges by weight.
#'   Default NULL (all edges).
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @export
#' @examples
#' \dontrun{
#' source_network(data, "coupling")
#' }
source_network <- function(data,
                           type = "coupling",
                           counting = "full",
                           similarity = "none",
                           threshold = 0,
                           min_occur = 1L,
                           top_n = NULL) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    type %in% c("coupling", "co_citation", "equivalence"),
    counting %in% position_independent_counts(),
    similarity %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  if (type == "coupling") {
    stopifnot("journal" %in% names(data), "references" %in% names(data))
    agg <- aggregate_by_entity(data, entity_field = "journal",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    B <- apply_counting(B, counting = counting, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", similarity = similarity,
                       threshold = threshold, top_n = top_n)

  } else if (type == "co_citation") {
    field <- if ("cited_journals" %in% names(data)) {
      "cited_journals"
    } else {
      stop("Column 'cited_journals' not found. ",
           "Parse reference strings to extract cited journals first.",
           call. = FALSE)
    }
    B <- build_bipartite(data, field = field, min_freq = min_occur)
    B <- apply_counting(B, counting = counting, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", similarity = similarity,
                       threshold = threshold, top_n = top_n)

  } else if (type == "equivalence") {
    stopifnot("journal" %in% names(data), "references" %in% names(data))
    agg <- aggregate_by_entity(data, entity_field = "journal",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    multiply_bipartite(B, mode = "rows", similarity = "cosine",
                       threshold = threshold, top_n = top_n)
  }
}
