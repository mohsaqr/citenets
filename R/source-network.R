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
#' @inheritParams author_network
#'
#' @return Depends on `format`: a `bibnets_network` data frame (default),
#'   a Gephi-ready data frame, an igraph graph, a cograph_network, or a
#'   sparse matrix.
#'
#' @export
#' @examples
#' data(biblio_data)
#' source_network(biblio_data, "coupling")
source_network <- function(data,
                           type = "coupling",
                           counting = "full",
                           similarity = "none",
                           threshold = 0,
                           min_occur = 1L,
                           top_n = NULL,
                           self_loops = FALSE,
                           deduplicate = TRUE,
                           format = "edgelist") {
  check_data(data, "id")
  check_choice(type, c("coupling", "co_citation", "equivalence"), "type")
  check_choice(counting, position_independent_counts(), "counting")
  check_choice(similarity, c("none", "association", "cosine", "jaccard",
                              "inclusion", "equivalence"), "similarity")
  check_format(format)

  result <- if (type == "coupling") {
    check_data(data, c("journal", "references"))
    agg <- aggregate_by_entity(data, entity_field = "journal",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    B <- apply_counting(B, counting = counting, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", similarity = similarity,
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)

  } else if (type == "co_citation") {
    field <- if ("cited_journals" %in% names(data)) {
      "cited_journals"
    } else {
      stop("Column 'cited_journals' not found. ",
           "Parse reference strings to extract cited journals first.",
           call. = FALSE)
    }
    B <- build_bipartite(data, field = field, min_freq = min_occur, deduplicate = deduplicate)
    B <- apply_counting(B, counting = counting, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", similarity = similarity,
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)

  } else {
    check_data(data, c("journal", "references"))
    agg <- aggregate_by_entity(data, entity_field = "journal",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    multiply_bipartite(B, mode = "rows", similarity = "cosine",
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)
  }

  as_bibnets_network(result, network_type = paste0("source_", type),
                      counting = counting, similarity = similarity,
                      format = format)
}
