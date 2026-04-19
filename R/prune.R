#' Prune a weighted edge list
#'
#' Reduces a weighted edge list by removing weak or excess edges.
#'
#' @param edges A data frame with at least columns `from`, `to`, and `weight`.
#' @param threshold Numeric. Keep only edges with `weight >= threshold`.
#' @param top_n Integer. For each node, keep only its `top_n` strongest edges.
#'   An edge is kept if it is in the top `top_n` for *either* endpoint.
#'
#' @return The filtered edge data frame (same columns as input).
#'
#' @export
#' @examples
#' edges <- data.frame(
#'   from   = c("A","A","A","B","B","C"),
#'   to     = c("B","C","D","C","D","D"),
#'   weight = c(5,  1,  2,  4,  1,  3)
#' )
#'
#' # Keep only edges with weight >= 3
#' prune(edges, threshold = 3)
#'
#' # Keep the 2 strongest edges per node
#' prune(edges, top_n = 2)
prune <- function(edges, threshold = NULL, top_n = NULL) {
  check_edges(edges)
  if (is.null(threshold) && is.null(top_n))
    stop("One of 'threshold' or 'top_n' must be specified.", call. = FALSE)
  if (!is.null(threshold) && !is.null(top_n))
    stop("Only one of 'threshold' or 'top_n' may be specified.", call. = FALSE)

  if (nrow(edges) == 0L) return(edges)

  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1L || threshold < 0)
      stop("'threshold' must be a non-negative number", call. = FALSE)
    edges <- edges[edges$weight >= threshold, ]
    edges <- edges[order(-edges$weight), ]
    rownames(edges) <- NULL
    return(edges)
  }

  if (!is.numeric(top_n) || length(top_n) != 1L || top_n < 0)
    stop("'top_n' must be a non-negative number", call. = FALSE)
  n <- as.integer(top_n)

  adj <- split(c(edges$weight, edges$weight), c(edges$from, edges$to))
  cutoff <- vapply(adj, function(w) {
    if (length(w) <= n) return(-Inf)
    sort(w, decreasing = TRUE)[n]
  }, numeric(1L))

  keep <- mapply(function(f, t, w) {
    w >= cutoff[f] | w >= cutoff[t]
  }, edges$from, edges$to, edges$weight)

  edges <- edges[keep, ]
  edges <- edges[order(-edges$weight), ]
  rownames(edges) <- NULL
  edges
}
