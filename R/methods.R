#' Print a citenets network edge list
#'
#' @param x A `citenets_network` data frame.
#' @param n Integer. Number of rows to show. Default 10.
#' @param ... Ignored.
#' @export
print.citenets_network <- function(x, n = 10L, ...) {
  nodes  <- length(unique(c(x$from, x$to)))
  edges  <- nrow(x)
  type   <- attr(x, "network_type")
  method <- attr(x, "counting")
  sim    <- attr(x, "similarity")

  cat(sprintf("# citenets network: %d nodes, %d edges", nodes, edges))
  if (!is.null(type))   cat(sprintf(" [%s]", type))
  if (!is.null(method)) cat(sprintf(" | counting: %s", method))
  if (!is.null(sim) && sim != "none") cat(sprintf(" | similarity: %s", sim))
  cat("\n")

  show <- min(n, edges)
  if (show > 0L) {
    print(as.data.frame(x)[seq_len(show), ], row.names = FALSE)
    if (edges > show)
      cat(sprintf("# ... %d more edges\n", edges - show))
  }
  invisible(x)
}


#' Summarise a citenets network
#'
#' @param object A `citenets_network` data frame.
#' @param ... Ignored.
#' @importFrom utils head
#' @export
summary.citenets_network <- function(object, ...) {
  nodes   <- unique(c(object$from, object$to))
  n_nodes <- length(nodes)
  n_edges <- nrow(object)
  max_possible <- n_nodes * (n_nodes - 1L) / 2L
  density <- if (max_possible > 0) round(n_edges / max_possible, 4) else NA_real_

  type   <- attr(object, "network_type")
  method <- attr(object, "counting")
  sim    <- attr(object, "similarity")

  cat("citenets network\n")
  cat(rep("-", 30L), "\n", sep = "")
  if (!is.null(type))   cat(sprintf("Type       : %s\n", type))
  if (!is.null(method)) cat(sprintf("Counting   : %s\n", method))
  if (!is.null(sim))    cat(sprintf("Similarity : %s\n", sim))
  cat(sprintf("Nodes      : %d\n", n_nodes))
  cat(sprintf("Edges      : %d\n", n_edges))
  cat(sprintf("Density    : %.4f\n", density))
  cat(sprintf("Weight     : min %.3g  median %.3g  max %.3g\n",
              min(object$weight), stats::median(object$weight),
              max(object$weight)))

  ## Top 5 nodes by degree
  deg <- sort(table(c(object$from, object$to)), decreasing = TRUE)
  top <- head(deg, 5L)
  cat(sprintf("Top nodes  : %s\n",
              paste(sprintf("%s(%d)", names(top), as.integer(top)),
                    collapse = ", ")))
  invisible(object)
}


## Internal helper — stamp a data.frame with the citenets_network class
## and network metadata attributes.
as_citenets_network <- function(edges, network_type = NULL,
                                counting = NULL, similarity = NULL) {
  class(edges) <- c("citenets_network", "data.frame")
  attr(edges, "network_type") <- network_type
  attr(edges, "counting")     <- counting
  attr(edges, "similarity")   <- similarity
  edges
}
