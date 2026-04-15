#' Prune a weighted edge list
#'
#' Reduces a weighted edge list by removing weak or excess edges.
#'
#' @param edges A data frame with at least columns `from`, `to`, and `weight`.
#' @param method Character. Pruning method:
#'   \describe{
#'     \item{`"threshold"`}{Remove edges with `weight < value`.}
#'     \item{`"top_n"`}{For each node, keep only its `value` strongest edges.
#'       An edge is kept if it is in the top `value` for *either* endpoint.}
#'   }
#' @param value Numeric. Threshold weight (for `"threshold"`) or number of
#'   edges to keep per node (for `"top_n"`).
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
#' prune(edges, method = "threshold", value = 3)
#'
#' # Keep the 2 strongest edges per node
#' prune(edges, method = "top_n", value = 2)
prune <- function(edges, method = "threshold", value) {
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges)),
    method %in% c("threshold", "top_n"),
    is.numeric(value), length(value) == 1L, value >= 0
  )

  if (nrow(edges) == 0L) return(edges)

  if (method == "threshold") {
    edges <- edges[edges$weight >= value, ]
    edges <- edges[order(-edges$weight), ]
    rownames(edges) <- NULL
    return(edges)
  }

  ## top_n: keep edge if it is in the top-value edges for either endpoint
  n <- as.integer(value)

  ## Build adjacency weight list in one O(m) pass via split(), then find
  ## each node's n-th strongest weight as its cutoff.
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
