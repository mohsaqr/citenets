#' Print a bibnets network edge list
#'
#' @param x A `bibnets_network` data frame.
#' @param n Integer. Number of rows to show. Default 10.
#' @param ... Ignored.
#' @return Invisibly returns `x`.
#' @examples
#' data(biblio_data)
#' edges <- author_network(biblio_data, "collaboration")
#' print(edges)
#' @export
print.bibnets_network <- function(x, n = 10L, ...) {
  nodes  <- length(unique(c(x$from, x$to)))
  edges  <- nrow(x)
  type   <- attr(x, "network_type")
  method <- attr(x, "counting")
  sim    <- attr(x, "similarity")

  header <- paste0("# bibnets network: ", type %||% "unknown",
                   " | ", format(nodes, big.mark = ","), " nodes",
                   " \u00b7 ", format(edges, big.mark = ","), " edges")
  if (!is.null(method)) header <- paste0(header, " | counting: ", method)
  if (!is.null(sim) && sim != "none") header <- paste0(header, " | similarity: ", sim)
  cat(header, "\n")

  show <- min(n, edges)
  if (show == 0L) return(invisible(x))

  d <- x[seq_len(show), , drop = FALSE]

  trunc_chr <- function(s, w) {
    long <- nchar(s) > w
    s[long] <- paste0(substr(s[long], 1L, w - 1L), "\u2026")
    s
  }

  max_w    <- 30L
  from_fmt <- trunc_chr(d$from, max_w)
  to_fmt   <- trunc_chr(d$to,   max_w)
  wt_fmt   <- formatC(d$weight, format = "fg", digits = 4L)
  ct_fmt   <- as.character(d$count)

  w_idx <- nchar(as.character(show))
  w_f   <- max(4L, max(nchar(from_fmt)))
  w_t   <- max(2L, max(nchar(to_fmt)))
  w_w   <- max(6L, max(nchar(wt_fmt)))
  w_c   <- max(5L, max(nchar(ct_fmt)))

  cat(sprintf("%*s  %-*s  %-*s  %*s  %*s\n",
              w_idx, "", w_f, "from", w_t, "to", w_w, "weight", w_c, "count"))

  for (i in seq_len(show)) {
    cat(sprintf("%*d  %-*s  %-*s  %*s  %*s\n",
                w_idx, i,
                w_f, from_fmt[i],
                w_t, to_fmt[i],
                w_w, wt_fmt[i],
                w_c, ct_fmt[i]))
  }

  if (edges > show)
    cat(sprintf("# ... %s more edges\n", format(edges - show, big.mark = ",")))

  invisible(x)
}


#' Summarise a bibnets network
#'
#' @param object A `bibnets_network` data frame.
#' @param ... Ignored.
#' @return Invisibly returns `object`.
#' @examples
#' data(biblio_data)
#' edges <- author_network(biblio_data, "collaboration")
#' summary(edges)
#' @importFrom utils head
#' @export
summary.bibnets_network <- function(object, ...) {
  nodes   <- unique(c(object$from, object$to))
  n_nodes <- length(nodes)
  n_edges <- nrow(object)
  max_possible <- n_nodes * (n_nodes - 1L) / 2L
  density <- if (max_possible > 0) round(n_edges / max_possible, 4) else NA_real_

  type   <- attr(object, "network_type")
  method <- attr(object, "counting")
  sim    <- attr(object, "similarity")

  cat("bibnets network\n")
  cat(rep("-", 30L), "\n", sep = "")
  if (!is.null(type))   cat(sprintf("Type       : %s\n", type))
  if (!is.null(method)) cat(sprintf("Counting   : %s\n", method))
  if (!is.null(sim))    cat(sprintf("Similarity : %s\n", sim))
  cat(sprintf("Nodes      : %d\n", n_nodes))
  cat(sprintf("Edges      : %d\n", n_edges))
  cat(sprintf("Density    : %.4f\n", density))

  if (n_edges == 0L) {
    cat("Weight     : (no edges)\n")
    invisible(object)
    return(invisible(object))
  }

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


as_bibnets_network <- function(edges, network_type = NULL,
                                counting = NULL, similarity = NULL,
                                format = "edgelist", directed = FALSE) {
  if (format == "gephi") {
    out <- edges
    names(out)[names(out) == "from"]   <- "Source"
    names(out)[names(out) == "to"]     <- "Target"
    names(out)[names(out) == "weight"] <- "Weight"
    names(out)[names(out) == "count"]  <- "Count"
    out[["Type"]] <- rep(if (directed) "Directed" else "Undirected", nrow(out))
    return(out)
  }

  if (format == "igraph") {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("Package 'igraph' is required for format = 'igraph'. ",
           "Install it with: install.packages('igraph')", call. = FALSE)
    }
    return(igraph::graph_from_data_frame(edges, directed = directed))
  }

  if (format == "cograph") {
    if (!requireNamespace("cograph", quietly = TRUE)) {
      stop("Package 'cograph' is required for format = 'cograph'. ",
           'Install it with: install.packages("cograph", ',
           'repos = "https://mohsaqr.r-universe.dev")', call. = FALSE)
    }
    return(cograph::as_cograph(edges, directed = directed))
  }

  if (format == "matrix") {
    return(edgelist_to_mat(edges, symmetric = !directed))
  }

  class(edges) <- c("bibnets_network", "data.frame")
  attr(edges, "network_type") <- network_type
  attr(edges, "counting")     <- counting
  attr(edges, "similarity")   <- similarity
  edges
}
