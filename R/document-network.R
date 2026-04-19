#' Build a document network
#'
#' Constructs a network between documents (papers) in the dataset.
#'
#' @param data A data frame with `id` and `references` (list-column).
#' @param type Character. Relationship type:
#'   \describe{
#'     \item{`"coupling"`}{Bibliographic coupling: documents linked when they
#'       share cited references.}
#'     \item{`"citation"`}{Direct citation: directed edges from citing to
#'       cited documents (internal citations only).}
#'     \item{`"co_citation"`}{Co-citation: documents linked when they are
#'       cited together by other documents in the dataset.}
#'     \item{`"equivalence"`}{Profile similarity of reference vectors.}
#'   }
#' @param counting Character. Counting method. Default `"full"`.
#'   Position-dependent methods are not applicable to document networks.
#' @param similarity Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum reference frequency. Default 1.
#' @param top_n Integer or NULL. Return only the top n edges by weight.
#'   Default NULL (all edges).
#' @inheritParams author_network
#'
#' @return Depends on `format`. For `type = "citation"`, edges are directed
#'   (from = citing, to = cited) with `weight` and `count` both 1.
#'
#' @export
#' @examples
#' data(biblio_data)
#' document_network(biblio_data, "coupling")
#' document_network(biblio_data, "coupling", counting = "strength")
document_network <- function(data,
                             type = "coupling",
                             counting = "full",
                             similarity = "none",
                             threshold = 0,
                             min_occur = 1L,
                             top_n = NULL,
                             self_loops = FALSE,
                             deduplicate = TRUE,
                             format = "edgelist") {
  check_data(data, c("id", "references"))
  check_choice(type, c("coupling", "citation", "co_citation", "equivalence"), "type")
  check_choice(counting, position_independent_counts(), "counting")
  check_choice(similarity, c("none", "association", "cosine", "jaccard",
                              "inclusion", "equivalence"), "similarity")
  check_format(format)

  result <- if (type == "citation") {
    edges <- build_direct_citation(data)
    if (!is.null(top_n) && nrow(edges) > 0) {
      freq <- sort(table(c(edges$from, edges$to)), decreasing = TRUE)
      top_nodes <- names(freq)[seq_len(min(top_n, length(freq)))]
      edges <- edges[edges$from %in% top_nodes & edges$to %in% top_nodes, ]
    }
    edges
  } else {
    B <- build_bipartite(data, field = "references", min_freq = min_occur, deduplicate = deduplicate)
    if (type == "coupling") {
      B <- apply_counting(B, counting = counting, network_type = "coupling")
      multiply_bipartite(B, mode = "rows", similarity = similarity,
                         threshold = threshold, top_n = top_n,
                         self_loops = self_loops)
    } else if (type == "co_citation") {
      B <- apply_counting(B, counting = counting, network_type = "symmetric")
      multiply_bipartite(B, mode = "columns", similarity = similarity,
                         threshold = threshold, top_n = top_n,
                         self_loops = self_loops)
    } else {
      multiply_bipartite(B, mode = "rows", similarity = "cosine",
                         threshold = threshold, top_n = top_n,
                         self_loops = self_loops)
    }
  }

  directed <- (type == "citation")
  as_bibnets_network(result, network_type = paste0("document_", type),
                      counting = counting, similarity = similarity,
                      format = format, directed = directed)
}


#' @keywords internal
build_direct_citation <- function(data) {
  ids <- as.character(data[["id"]])
  refs_list <- data[["references"]]

  citing <- rep(ids, lengths(refs_list))
  cited <- unlist(refs_list, use.names = FALSE)

  keep <- cited %in% ids & !is.na(cited)
  citing <- citing[keep]
  cited <- cited[keep]

  if (length(citing) == 0L) {
    return(data.frame(
      from = character(0), to = character(0),
      weight = numeric(0), count = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    from = citing, to = cited,
    weight = rep(1, length(citing)),
    count = rep(1L, length(citing)),
    stringsAsFactors = FALSE
  )
}
