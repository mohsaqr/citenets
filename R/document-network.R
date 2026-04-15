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
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'   For `type = "citation"`, the edges are directed (from = citing,
#'   to = cited).
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
                             top_n = NULL) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "references" %in% names(data),
    type %in% c("coupling", "citation", "co_citation", "equivalence"),
    counting %in% position_independent_counts(),
    similarity %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  if (type == "citation") {
    edges <- build_direct_citation(data)
    if (!is.null(top_n) && nrow(edges) > 0) {
      ## Top-n nodes by frequency (cited + citing count)
      all_nodes <- c(edges$from, edges$to)
      freq <- sort(table(all_nodes), decreasing = TRUE)
      top_nodes <- names(freq)[seq_len(min(top_n, length(freq)))]
      edges <- edges[edges$from %in% top_nodes & edges$to %in% top_nodes, ]
    }
    return(edges)
  }

  B <- build_bipartite(data, field = "references", min_freq = min_occur)

  if (type == "coupling") {
    B <- apply_counting(B, counting = counting, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", similarity = similarity,
                       threshold = threshold, top_n = top_n)

  } else if (type == "co_citation") {
    B <- apply_counting(B, counting = counting, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", similarity = similarity,
                       threshold = threshold, top_n = top_n)

  } else if (type == "equivalence") {
    multiply_bipartite(B, mode = "rows", similarity = "cosine",
                       threshold = threshold, top_n = top_n)
  }
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
      weight = numeric(0), count = integer(0), shared = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    from = citing, to = cited,
    weight = rep(1, length(citing)),
    count = rep(1L, length(citing)),
    shared = rep(1L, length(citing)),
    stringsAsFactors = FALSE
  )
}
