#' Convert edge data frame to igraph
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns,
#'   as returned by any network function in citenets.
#' @param directed Logical. Default `FALSE`.
#'
#' @return An igraph graph object.
#'
#' @export
#' @examples
#' \dontrun{
#' data(biblio_data)
#' edges <- author_network(biblio_data, "collaboration")
#' g <- to_igraph(edges)
#' }
to_igraph <- function(edges, directed = FALSE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required. Install it with: ",
         "install.packages('igraph')", call. = FALSE)
  }
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )
  igraph::graph_from_data_frame(edges, directed = directed)
}


#' Convert edge data frame to tbl_graph
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns.
#' @param directed Logical. Default `FALSE`.
#'
#' @return A tbl_graph object (tidygraph).
#'
#' @export
#' @examples
#' \dontrun{
#' data(biblio_data)
#' edges <- keyword_network(biblio_data)
#' tg <- to_tbl_graph(edges)
#' }
to_tbl_graph <- function(edges, directed = FALSE) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' is required. Install it with: ",
         "install.packages('tidygraph')", call. = FALSE)
  }
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )
  tidygraph::as_tbl_graph(to_igraph(edges, directed = directed))
}


#' Convert edge data frame to adjacency matrix
#'
#' @param edges A data frame with `from`, `to`, `weight` columns.
#' @param symmetric Logical. If `TRUE` (default), produces a symmetric matrix.
#'
#' @return A sparse Matrix.
#'
#' @export
#' @examples
#' data(biblio_data)
#' edges <- reference_network(biblio_data, min_occur = 2)
#' to_matrix(edges)
to_matrix <- function(edges, symmetric = TRUE) {
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )
  edgelist_to_mat(edges, symmetric = symmetric)
}
