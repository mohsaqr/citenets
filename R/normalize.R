#' Normalize a co-occurrence matrix
#'
#' Applies a similarity normalization to a square co-occurrence matrix.
#' The diagonal of the input matrix is used as the total occurrence count
#' for each item. Operates entirely in sparse representation.
#'
#' @param A A square symmetric matrix (dense or sparse) representing
#'   co-occurrence counts.
#' @param method Character. Normalization method:
#'   \describe{
#'     \item{`"none"`}{No normalization. Returns raw co-occurrence counts.}
#'     \item{`"association"`}{Association strength (probabilistic affinity
#'       index). \eqn{s_{ij} = c_{ij} / (w_i \cdot w_j)}.
#'       Recommended by van Eck & Waltman (2009) as the best normalization
#'       for co-occurrence data.}
#'     \item{`"cosine"`}{Salton's cosine.
#'       \eqn{s_{ij} = c_{ij} / \sqrt{w_i \cdot w_j}}.}
#'     \item{`"jaccard"`}{Jaccard index.
#'       \eqn{s_{ij} = c_{ij} / (w_i + w_j - c_{ij})}.}
#'     \item{`"inclusion"`}{Inclusion index (Simpson coefficient).
#'       \eqn{s_{ij} = c_{ij} / \min(w_i, w_j)}.}
#'     \item{`"equivalence"`}{Equivalence index (Salton's cosine squared).
#'       \eqn{s_{ij} = c_{ij}^2 / (w_i \cdot w_j)}.}
#'   }
#'
#' @return A normalized sparse matrix of the same dimensions.
#'
#' @references
#' van Eck, N. J., & Waltman, L. (2009). How to normalize co-occurrence
#' data? An analysis of some well-known similarity measures. *Journal of the
#' American Society for Information Science and Technology*, 60(8),
#' 1635--1651. \doi{10.1002/asi.21075}
#'
#' @export
#' @examples
#' # Create a small co-occurrence matrix
#' A <- matrix(c(10, 3, 1, 3, 8, 2, 1, 2, 5), nrow = 3,
#'             dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
#' normalize(A, "association")
#' normalize(A, "cosine")
#' normalize(A, "jaccard")
normalize <- function(A, method = "none") {
  stopifnot(
    is.matrix(A) || inherits(A, "Matrix"),
    nrow(A) == ncol(A),
    method %in% c("none", "association", "cosine", "jaccard",
                   "inclusion", "equivalence")
  )

  if (method == "none") return(A)

  ## Diagonal = total occurrence count per item
  D <- Matrix::diag(A)
  D[D == 0] <- 1

  ## For small matrices (< 500), use dense path (simpler, fast enough)
  if (nrow(A) < 500) {
    return(normalize_dense(A, method, D))
  }

  ## Sparse path: operate on triplets, never allocate dense outer product
  normalize_sparse(A, method, D)
}


#' @keywords internal
normalize_dense <- function(A, method, D) {
  A <- as.matrix(A)

  S <- switch(method,
    association = {
      A / outer(D, D, `*`)
    },
    cosine = {
      A / outer(D, D, function(a, b) sqrt(a * b))
    },
    jaccard = {
      denom <- outer(D, D, `+`) - A
      denom[denom == 0] <- 1
      A / denom
    },
    inclusion = {
      denom <- outer(D, D, pmin)
      denom[denom == 0] <- 1
      A / denom
    },
    equivalence = {
      A^2 / outer(D, D, `*`)
    }
  )

  S[is.nan(S)] <- 0
  S[is.infinite(S)] <- 0
  diag(S) <- 0
  Matrix::Matrix(S, sparse = TRUE)
}


#' @keywords internal
normalize_sparse <- function(A, method, D) {
  ## Extract non-zero triplets
  triplets <- Matrix::summary(A)
  i <- triplets$i
  j <- triplets$j
  x <- triplets$x

  ## Remove diagonal
  off <- i != j
  i <- i[off]; j <- j[off]; x <- x[off]

  ## Upper triangle only (symmetric)
  upper <- i < j
  i <- i[upper]; j <- j[upper]; x <- x[upper]

  Di <- D[i]
  Dj <- D[j]

  new_x <- switch(method,
    association = x / (Di * Dj),
    cosine = x / sqrt(Di * Dj),
    jaccard = x / (Di + Dj - x),
    inclusion = x / pmin(Di, Dj),
    equivalence = x^2 / (Di * Dj)
  )

  new_x[is.nan(new_x)] <- 0
  new_x[is.infinite(new_x)] <- 0

  ## Build symmetric sparse matrix from upper triangle
  n <- nrow(A)
  nms <- rownames(A)
  S <- Matrix::sparseMatrix(
    i = c(i, j), j = c(j, i), x = c(new_x, new_x),
    dims = c(n, n), dimnames = list(nms, nms)
  )

  S
}
