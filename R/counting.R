#' Compute positional author weights for a single paper
#'
#' Given the number of authors and their positions, returns a weight vector
#' using the specified counting method. All position-dependent methods
#' normalize weights to sum to 1 per paper.
#'
#' @param n Integer. Number of authors.
#' @param count Character. Counting method.
#' @param position_weights Numeric vector. Custom weights for
#'   `count = "position_weighted"`. Default `c(1, 0.8, 0.6, 0.4)`.
#' @param first_last_weight Numeric. Multiplier for first/last authors
#'   when `count = "first_last"`. Default 2.
#'
#' @return Numeric vector of length `n`, summing to 1.
#' @keywords internal
author_weights <- function(n, count = "fractional",
                           position_weights = c(1, 0.8, 0.6, 0.4),
                           first_last_weight = 2) {
  if (n <= 0) return(numeric(0))
  if (n == 1) return(1)

  pos <- seq_len(n)

  w <- switch(count,

    ## --- Position-independent ---
    full = rep(1, n),

    fractional = rep(1 / n, n),

    paper = {
      ## Batagelj: total network contribution per paper = 1
      ## n*(n-1)/2 links, so each entry = sqrt(2/(n*(n-1)))
      ## But for weights vector: equal shares, network-level normalization
      ## handled in multiply step
      rep(1 / n, n)
    },

    ## --- Position-dependent ---
    harmonic = {
      ## Hagen (2008): weight_i = (1/i) / H_n
      raw <- 1 / pos
      raw / sum(raw)
    },

    arithmetic = {
      ## Linear decrease: weight_i = (n - i + 1) / sum(1:n)
      raw <- n - pos + 1
      raw / sum(raw)
    },

    geometric = {
      ## Geometric decay: weight_i = 0.5^(i-1), normalized
      raw <- 0.5^(pos - 1)
      raw / sum(raw)
    },

    adaptive_geometric = {
      ## Liu & Fang (2023): ratio of first to last = n
      ## r = n^(1/(n-1)), weight_i = r^(n-i) / sum(r^(n-j))
      if (n == 2) {
        c(2/3, 1/3)
      } else {
        r <- n^(1 / (n - 1))
        raw <- r^(n - pos)
        raw / sum(raw)
      }
    },

    golden = {
      ## Abbas (2011): golden ratio phi = (1+sqrt(5))/2
      phi <- (1 + sqrt(5)) / 2
      raw <- phi^(n - pos)
      raw / sum(raw)
    },

    first = {
      ## Straight first-author counting
      w <- rep(0, n)
      w[1] <- 1
      w
    },

    last = {
      ## Last-author counting
      w <- rep(0, n)
      w[n] <- 1
      w
    },

    first_last = {
      ## First and last elevated, middle share rest
      w <- rep(0, n)
      if (n == 2) {
        w <- c(0.5, 0.5)
      } else {
        elevated <- first_last_weight / (2 * first_last_weight + (n - 2))
        middle <- 1 / (2 * first_last_weight + (n - 2))
        w[1] <- elevated
        w[n] <- elevated
        w[2:(n - 1)] <- middle
      }
      w
    },

    position_weighted = {
      ## Custom weights vector, extended if needed
      raw <- vapply(pos, function(i) {
        if (i <= length(position_weights)) position_weights[i]
        else position_weights[length(position_weights)]
      }, numeric(1))
      raw / sum(raw)
    },

    stop(sprintf("Unknown counting method: '%s'", count), call. = FALSE)
  )

  w
}


#' Build a weighted bipartite matrix using positional counting
#'
#' For position-dependent counting methods, this replaces the binary bipartite
#' matrix entries with positional weights derived from author order.
#'
#' @param data A data frame with `id` and `authors` (list-column where
#'   author order is preserved).
#' @param count Character. Counting method.
#' @param position_weights Numeric vector for `count = "position_weighted"`.
#' @param first_last_weight Numeric for `count = "first_last"`.
#'
#' @return A sparse weighted bipartite matrix (works x authors).
#' @keywords internal
build_author_bipartite <- function(data, count = "full",
                                   position_weights = c(1, 0.8, 0.6, 0.4),
                                   first_last_weight = 2) {
  ids <- as.character(data[["id"]])
  authors_list <- data[["authors"]]

  ## Expand to long form with positions
  n_per_paper <- lengths(authors_list)
  work_idx <- rep(seq_along(ids), n_per_paper)
  author_names <- unlist(authors_list, use.names = FALSE)
  positions <- unlist(lapply(n_per_paper, seq_len), use.names = FALSE)

  ## Drop NA/empty
  keep <- !is.na(author_names) & nchar(author_names) > 0
  work_idx <- work_idx[keep]
  author_names <- author_names[keep]
  positions <- positions[keep]
  n_per <- n_per_paper[work_idx]

  ## Compute weights
  weights <- mapply(
    function(pos, n) {
      author_weights(n, count = count,
                     position_weights = position_weights,
                     first_last_weight = first_last_weight)[pos]
    },
    positions, n_per
  )

  ## Drop zero-weight entries (e.g., "first" or "last" counting)
  nonzero <- weights > 0
  work_idx <- work_idx[nonzero]
  author_names <- author_names[nonzero]
  weights <- weights[nonzero]

  ## Map to column indices
  unique_authors <- sort(unique(author_names))
  col_idx <- match(author_names, unique_authors)

  Matrix::sparseMatrix(
    i = work_idx,
    j = col_idx,
    x = weights,
    dims = c(length(ids), length(unique_authors)),
    dimnames = list(ids, unique_authors)
  )
}


#' Apply counting weights to a generic bipartite matrix
#'
#' For position-independent counting of non-author fields (references,
#' keywords, etc.). Modifies the bipartite matrix row weights.
#'
#' @param B A sparse binary bipartite matrix (works x entities).
#' @param count Character. One of `"full"`, `"fractional"`, `"paper"`,
#'   `"strength"`.
#' @param network_type Character. `"symmetric"` or `"coupling"`.
#'
#' @return A weighted sparse matrix.
#' @keywords internal
apply_counting <- function(B, count = "full",
                           network_type = "symmetric") {
  if (count == "full") return(B)

  n_per_work <- Matrix::rowSums(B > 0)
  n_per_work[n_per_work == 0] <- 1

  if (count == "fractional") {
    if (network_type == "symmetric") {
      ## Perianes-Rodriguez: each entity's total per paper = 1
      ## Link weight = 1/(n-1) per shared paper
      w <- ifelse(n_per_work > 1, 1 / (n_per_work - 1), 1)
    } else {
      ## Coupling/other: weight = 1/n per entity
      w <- 1 / n_per_work
    }
    B <- Matrix::Diagonal(x = sqrt(w)) %*% B

  } else if (count == "paper") {
    if (network_type == "symmetric") {
      w <- ifelse(
        n_per_work > 1,
        2 / (n_per_work * (n_per_work - 1)),
        1
      )
    } else {
      w <- 1 / n_per_work
    }
    B <- Matrix::Diagonal(x = sqrt(w)) %*% B

  } else if (count == "strength") {
    n_works <- nrow(B)
    entity_freq <- Matrix::colSums(B > 0)
    entity_freq[entity_freq == 0] <- 1
    idf <- log(n_works / entity_freq)
    row_w <- 1 / n_per_work

    B <- Matrix::Diagonal(x = sqrt(row_w)) %*% B %*%
      Matrix::Diagonal(x = sqrt(idf))
  }

  B
}


#' List of all available counting methods
#'
#' @return Character vector of method names.
#' @keywords internal
position_independent_counts <- function() {
  c("full", "fractional", "paper", "strength")
}

#' @keywords internal
position_dependent_counts <- function() {
  c("harmonic", "arithmetic", "geometric", "adaptive_geometric",
    "golden", "first", "last", "first_last", "position_weighted")
}

#' @keywords internal
all_counts <- function() {
  c(position_independent_counts(), position_dependent_counts())
}
