#' Construct a co-occurrence network via two-mode multiplication
#'
#' The unified engine for all bibliometric networks. Operates entirely in
#' sparse representation — never allocates a dense n x n matrix.
#'
#' @param B A sparse bipartite matrix (works x entities), already weighted
#'   by the counting method.
#' @param mode Character. `"columns"` for column-mode co-occurrence
#'   (e.g., co-citation), `"rows"` for row-mode (e.g., coupling).
#' @param similarity Character. Normalization method (see [normalize()]).
#' @param threshold Numeric. Minimum edge weight to retain.
#' @param top_n Integer or `NULL`. If specified, keep only the top `n`
#'   most frequent nodes and return all edges among them.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#' @keywords internal
multiply_bipartite <- function(B, mode = "columns",
                               similarity = "none",
                               threshold = 0,
                               top_n = NULL) {
  ## Weighted co-occurrence matrix (sparse)
  if (mode == "columns") {
    A <- Matrix::crossprod(B)
  } else {
    A <- Matrix::tcrossprod(B)
  }

  ## Raw binary co-occurrence for the count/shared columns
  B_bin <- (B > 0) * 1
  if (mode == "columns") {
    A_raw <- Matrix::crossprod(B_bin)
  } else {
    A_raw <- Matrix::tcrossprod(B_bin)
  }

  ## Top-n node filter: keep only the most frequent entities
  if (!is.null(top_n)) {
    freq <- Matrix::diag(A)
    if (length(freq) > top_n) {
      keep_idx <- order(-freq)[seq_len(top_n)]
      A <- A[keep_idx, keep_idx]
      A_raw <- A_raw[keep_idx, keep_idx]
    }
  }

  ## Normalize weighted matrix (or just zero the diagonal)
  if (similarity != "none") {
    A_norm <- normalize(A, method = similarity)
  } else {
    Matrix::diag(A) <- 0
    A_norm <- A
  }

  ## Extract upper-triangle edges directly from sparse (no dense conversion)
  triplets <- Matrix::summary(A_norm)
  i <- triplets$i
  j <- triplets$j
  x <- triplets$x

  ## Remove diagonal, lower triangle, and zeros
  keep <- i < j & x != 0
  i <- i[keep]; j <- j[keep]; x <- x[keep]

  if (length(i) == 0L) {
    return(data.frame(
      from = character(0), to = character(0),
      weight = numeric(0), count = integer(0), shared = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  ## Threshold filter early (before name lookup)
  if (threshold > 0) {
    keep <- x >= threshold
    i <- i[keep]; j <- j[keep]; x <- x[keep]
  }

  if (length(i) == 0L) {
    return(data.frame(
      from = character(0), to = character(0),
      weight = numeric(0), count = integer(0), shared = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  node_names <- rownames(A_norm)
  if (is.null(node_names)) node_names <- as.character(seq_len(nrow(A_norm)))

  ## Look up raw counts for matched edges (vectorized sparse access)
  raw_counts <- A_raw[cbind(i, j)]

  edges <- data.frame(
    from = node_names[i],
    to = node_names[j],
    weight = x,
    count = as.integer(raw_counts),
    shared = as.integer(raw_counts),
    stringsAsFactors = FALSE
  )

  ## Sort by weight descending
  edges <- edges[order(-edges$weight), ]
  rownames(edges) <- NULL
  edges
}
