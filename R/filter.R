#' Filter edges to top-n nodes
#'
#' Keeps only edges between the most frequent nodes. Node frequency is
#' determined by how many edges each node participates in.
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns.
#' @param n Integer. Number of top nodes to keep.
#'
#' @return A filtered data frame with edges among the top `n` nodes.
#'
#' @export
#' @examples
#' data(biblio_data)
#' edges <- author_network(biblio_data, "collaboration")
#'
#' # Keep only edges among the top 3 most connected authors
#' filter_top(edges, 3)
filter_top <- function(edges, n) {
  check_edges(edges)
  if (!is.numeric(n) || length(n) != 1 || n <= 0)
    stop("'n' must be a positive number", call. = FALSE)

  if (nrow(edges) == 0L) return(edges)

  n <- as.integer(n)

  ## Node frequency = number of edges per node
  all_nodes <- c(edges$from, edges$to)
  freq <- sort(table(all_nodes), decreasing = TRUE)
  top_nodes <- names(freq)[seq_len(min(n, length(freq)))]

  edges <- edges[edges$from %in% top_nodes & edges$to %in% top_nodes, ]
  edges <- edges[order(-edges$weight), ]
  rownames(edges) <- NULL
  edges
}
