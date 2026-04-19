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
#' data(open_alex_gold_open_access_learning_analytics)
#' country_network(open_alex_gold_open_access_learning_analytics, "collaboration")
country_network <- function(data,
                            type = "collaboration",
                            counting = "full",
                            similarity = "none",
                            threshold = 0,
                            min_occur = 1L,
                            attention = NULL,
                            top_n = NULL,
                            self_loops = FALSE,
                            deduplicate = TRUE,
                            format = "edgelist") {
  check_data(data, c("id", "countries"))
  check_choice(similarity, c("none", "association", "cosine", "jaccard",
                              "inclusion", "equivalence"), "similarity")
  check_format(format)

  if (!is.null(attention)) {
    check_choice(attention, all_attention_methods(), "attention")
    B <- build_author_bipartite(data, field = "countries",
                                counting = paste0("attention_", attention),
                                deduplicate = deduplicate)
    result <- multiply_bipartite(B, mode = "columns", similarity = similarity,
                                 threshold = threshold, top_n = top_n,
                                 self_loops = self_loops)
    return(as_bibnets_network(result,
      network_type = paste0("country_attention_", attention),
      counting = attention, similarity = similarity, format = format))
  }

  check_choice(type, c("collaboration", "coupling", "equivalence"), "type")
  check_choice(counting, position_independent_counts(), "counting")

  result <- if (type == "collaboration") {
    B <- build_bipartite(data, field = "countries", min_freq = min_occur, deduplicate = deduplicate)
    B <- apply_counting(B, counting = counting, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", similarity = similarity,
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)

  } else if (type == "coupling") {
    if (!"references" %in% names(data))
      stop("Column 'references' not found. Required for type = 'coupling'.", call. = FALSE)
    agg <- aggregate_by_entity(data, entity_field = "countries",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    B <- apply_counting(B, counting = counting, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", similarity = similarity,
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)

  } else {
    B <- build_bipartite(data, field = "countries", min_freq = min_occur, deduplicate = deduplicate)
    multiply_bipartite(B, mode = "columns", similarity = "cosine",
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)
  }

  as_bibnets_network(result, network_type = paste0("country_", type),
                      counting = counting, similarity = similarity,
                      format = format)
}
