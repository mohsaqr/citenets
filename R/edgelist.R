#' Convert a sparse square matrix to a tidy edge list
#'
#' Extracts non-zero entries directly from the sparse representation —
#' no dense matrix allocation. For undirected networks, only the upper
#' triangle is returned.
#'
#' @param A A square sparse or dense matrix.
#' @param directed Logical. If `FALSE` (default), returns only upper-triangle
#'   entries. If `TRUE`, returns all non-zero off-diagonal entries.
#'
#' @return A data frame with columns `from`, `to`, `weight`, sorted by
#'   descending weight.
#'
#' @keywords internal
mat_to_edgelist <- function(A, directed = FALSE) {
  stopifnot(nrow(A) == ncol(A))

  node_names <- rownames(A)
  if (is.null(node_names)) {
    node_names <- as.character(seq_len(nrow(A)))
  }

  ## Extract triplets directly from sparse matrix
  if (inherits(A, "Matrix")) {
    triplets <- Matrix::summary(A)
    i <- triplets$i
    j <- triplets$j
    x <- triplets$x
  } else {
    idx <- which(A != 0, arr.ind = TRUE)
    if (nrow(idx) == 0L) {
      return(data.frame(from = character(0), to = character(0),
                        weight = numeric(0), stringsAsFactors = FALSE))
    }
    i <- idx[, 1]
    j <- idx[, 2]
    x <- A[idx]
  }

  ## Remove diagonal
  keep <- i != j
  i <- i[keep]; j <- j[keep]; x <- x[keep]

  ## Upper triangle only for undirected
  if (!directed) {
    keep <- i < j
    i <- i[keep]; j <- j[keep]; x <- x[keep]
  }

  if (length(i) == 0L) {
    return(data.frame(from = character(0), to = character(0),
                      weight = numeric(0), stringsAsFactors = FALSE))
  }

  edges <- data.frame(
    from = node_names[i],
    to = node_names[j],
    weight = x,
    stringsAsFactors = FALSE
  )

  edges[order(-edges$weight), ]
}


#' Convert edge list to a square sparse matrix
#'
#' @param edges A data frame with columns `from`, `to`, `weight`.
#' @param nodes Optional character vector of node names. If `NULL`,
#'   derived from the edge list.
#' @param symmetric Logical. If `TRUE` (default), the matrix is made
#'   symmetric.
#'
#' @return A sparse `dgCMatrix`.
#' @keywords internal
edgelist_to_mat <- function(edges, nodes = NULL, symmetric = TRUE) {
  if (is.null(nodes)) {
    nodes <- sort(unique(c(edges$from, edges$to)))
  }

  i <- match(edges$from, nodes)
  j <- match(edges$to, nodes)
  n <- length(nodes)

  A <- Matrix::sparseMatrix(
    i = i, j = j, x = edges$weight,
    dims = c(n, n),
    dimnames = list(nodes, nodes)
  )

  if (symmetric) {
    A <- A + Matrix::t(A)
    Matrix::diag(A) <- Matrix::diag(A) / 2
  }

  A
}
