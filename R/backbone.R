#' Extract network backbone using the disparity filter
#'
#' Applies the Serrano et al. (2009) disparity filter to a weighted edge list.
#' For each edge, it computes an alpha (p-value) from both endpoints and keeps
#' the edge if it is statistically significant from at least one endpoint.
#'
#' The null model asks: given that node \eqn{i} has total strength \eqn{s_i}
#' distributed uniformly across \eqn{k_i} edges, what is the probability that
#' a single edge weight is as large as \eqn{w_{ij}}? The answer is
#' \deqn{\alpha_{ij} = \left(1 - \frac{w_{ij}}{s_i}\right)^{k_i - 1}}
#'
#' An edge is retained if \eqn{\min(\alpha_{ij}, \alpha_{ji}) < \alpha}.
#' Nodes with only one edge always have \eqn{\alpha = 0} and are always kept.
#'
#' @param edges A data frame with at least columns `from`, `to`, and `weight`.
#'   Must be an undirected edge list (each pair appears once).
#' @param alpha Numeric. Significance threshold in (0, 1). Default `0.05`.
#'
#' @return The filtered edge data frame with an added `alpha` column (the
#'   minimum alpha from the two endpoints).
#'
#' @references
#' Serrano, M. Á., Boguñá, M., & Vespignani, A. (2009). Extracting the
#' multiscale backbone of complex weighted networks. *PNAS*, 106(16),
#' 6483–6488.
#'
#' @export
#' @examples
#' edges <- data.frame(
#'   from   = c("A", "A", "A", "B", "C"),
#'   to     = c("B", "C", "D", "C", "D"),
#'   weight = c(10,   1,   1,   8,   1)
#' )
#' backbone(edges, alpha = 0.05)
backbone <- function(edges, alpha = 0.05) {
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges)),
    is.numeric(alpha), length(alpha) == 1L,
    alpha > 0, alpha < 1
  )

  if (nrow(edges) == 0L) {
    edges$alpha <- numeric(0)
    return(edges)
  }

  ## Build strength (sum of weights) and degree (number of edges) per node
  ## from the undirected edge list
  all_nodes <- unique(c(edges$from, edges$to))

  ## Vectorised: compute strength and degree for every node at once
  strength <- vapply(all_nodes, function(v) {
    sum(edges$weight[edges$from == v | edges$to == v])
  }, numeric(1L))
  degree <- vapply(all_nodes, function(v) {
    sum(edges$from == v | edges$to == v)
  }, integer(1L))

  names(strength) <- all_nodes
  names(degree)   <- all_nodes

  ## Alpha from node i for edge (i,j): (1 - w/s_i)^(k_i - 1)
  ## Nodes with degree 1 always get alpha = 0 (always kept)
  compute_alpha <- function(node, w) {
    s <- strength[node]
    k <- degree[node]
    if (k <= 1L || s == 0) return(0)
    (1 - w / s) ^ (k - 1L)
  }

  alpha_from <- mapply(compute_alpha, edges$from, edges$weight)
  alpha_to   <- mapply(compute_alpha, edges$to,   edges$weight)

  edge_alpha <- pmin(alpha_from, alpha_to)

  edges$alpha <- edge_alpha
  edges[edge_alpha < alpha, ]
}
