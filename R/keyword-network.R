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
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum keyword frequency. Default 1.
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
#' keyword_network(biblio_data)
#' keyword_network(biblio_data, similarity = "association")
keyword_network <- function(data,
                            field = "keywords",
                            counting = "full",
                            similarity = "none",
                            threshold = 0,
                            min_occur = 1L,
                            attention = NULL,
                            top_n = NULL,
                            self_loops = FALSE,
                            deduplicate = TRUE,
                            format = "edgelist") {
  check_data(data, c("id", field))
  data <- ensure_list_column(data, field)
  check_choice(similarity, c("none", "association", "cosine", "jaccard",
                              "inclusion", "equivalence"), "similarity")
  check_format(format)

  if (!is.null(attention)) {
    check_choice(attention, all_attention_methods(), "attention")
    B <- build_author_bipartite(data, field = field,
                                counting = paste0("attention_", attention),
                                deduplicate = deduplicate)
    result <- multiply_bipartite(B, mode = "columns", similarity = similarity,
                                 threshold = threshold, top_n = top_n,
                                 self_loops = self_loops)
    return(as_bibnets_network(result,
      network_type = paste0("keyword_attention_", attention),
      counting = attention, similarity = similarity, format = format))
  }

  check_choice(counting, position_independent_counts(), "counting")
  B <- build_bipartite(data, field = field, min_freq = min_occur, deduplicate = deduplicate)
  B <- apply_counting(B, counting = counting, network_type = "symmetric")
  result <- multiply_bipartite(B, mode = "columns", similarity = similarity,
                               threshold = threshold, top_n = top_n,
                               self_loops = self_loops)
  as_bibnets_network(result, network_type = "keyword_co_occurrence",
                      counting = counting, similarity = similarity,
                      format = format)
}
