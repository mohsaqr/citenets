#' Build an author network
#'
#' Constructs a network between authors using one of four relationship types
#' and any of 13 counting methods, including 9 position-dependent methods
#' that respect author byline order.
#'
#' @param data A data frame with at least `id` and `authors` (list-column,
#'   order preserved). For coupling/co-citation, also needs `references`.
#' @param type Character. Relationship type:
#'   \describe{
#'     \item{`"collaboration"`}{Co-authorship: authors linked when they
#'       co-author a publication.}
#'     \item{`"coupling"`}{Bibliographic coupling aggregated at author level:
#'       authors linked when they cite the same references.}
#'     \item{`"co_citation"`}{Author co-citation: authors linked when they
#'       are cited together by the same paper. Requires a
#'       `cited_first_authors` list-column.}
#'     \item{`"equivalence"`}{Profile similarity: cosine similarity of
#'       authors' full collaboration/citation profiles.}
#'   }
#' @param count Character. Counting method. Position-independent methods
#'   (`"full"`, `"fractional"`, `"paper"`, `"strength"`) work for all types.
#'   Position-dependent methods (`"harmonic"`, `"arithmetic"`, `"geometric"`,
#'   `"adaptive_geometric"`, `"golden"`, `"first"`, `"last"`,
#'   `"first_last"`, `"position_weighted"`) are available for
#'   `type = "collaboration"`.
#' @param measure Character. Similarity measure: `"none"`, `"association"`,
#'   `"cosine"`, `"jaccard"`, `"inclusion"`, `"equivalence"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum number of papers for an author to be
#'   included. Default 1.
#' @param position_weights Numeric vector. Custom weights for
#'   `count = "position_weighted"`. Default `c(1, 0.8, 0.6, 0.4)`.
#' @param first_last_weight Numeric. Multiplier for `count = "first_last"`.
#'   Default 2.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'
#' @references
#' Hagen, N. T. (2008). Harmonic allocation of authorship credit.
#' *Scientometrics*, 84(3), 785--793. \doi{10.1007/s11192-009-0129-4}
#'
#' Liu, W., & Fang, H. (2023). A geometric counting method adaptive to the
#' author number. *Journal of Informetrics*, 17(2), 101397.
#' \doi{10.1016/j.joi.2023.101397}
#'
#' @export
#' @examples
#' data(biblio_data)
#' author_network(biblio_data, "collaboration")
#' author_network(biblio_data, "collaboration", count = "harmonic")
#' author_network(biblio_data, "collaboration", count = "geometric",
#'                measure = "association")
author_network <- function(data,
                           type = "collaboration",
                           count = "full",
                           measure = "none",
                           threshold = 0,
                           min_occur = 1L,
                           position_weights = c(1, 0.8, 0.6, 0.4),
                           first_last_weight = 2) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "authors" %in% names(data),
    type %in% c("collaboration", "coupling", "co_citation", "equivalence"),
    count %in% all_counts(),
    measure %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  is_positional <- count %in% position_dependent_counts()

  if (type == "collaboration") {
    if (is_positional) {
      B <- build_author_bipartite(
        data, count = count,
        position_weights = position_weights,
        first_last_weight = first_last_weight
      )
      ## For positional: B already carries weights, multiply directly
      ## Measure "cosine" for equivalence type
      m <- if (type == "equivalence") "cosine" else measure
      multiply_bipartite(B, mode = "columns", measure = m,
                         threshold = threshold)
    } else {
      B <- build_bipartite(data, field = "authors", min_freq = min_occur)
      B <- apply_counting(B, count = count, network_type = "symmetric")
      multiply_bipartite(B, mode = "columns", measure = measure,
                         threshold = threshold)
    }

  } else if (type == "coupling") {
    stopifnot("references" %in% names(data))
    agg <- aggregate_by_entity(data, entity_field = "authors",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    ct <- if (is_positional) "full" else count
    B <- apply_counting(B, count = ct, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", measure = measure,
                       threshold = threshold)

  } else if (type == "co_citation") {
    field <- if ("cited_first_authors" %in% names(data)) {
      "cited_first_authors"
    } else {
      stop("Column 'cited_first_authors' not found. ",
           "Parse reference strings to extract cited authors first.",
           call. = FALSE)
    }
    B <- build_bipartite(data, field = field, min_freq = min_occur)
    ct <- if (is_positional) "full" else count
    B <- apply_counting(B, count = ct, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", measure = measure,
                       threshold = threshold)

  } else if (type == "equivalence") {
    B <- build_bipartite(data, field = "authors", min_freq = min_occur)
    ## Equivalence = cosine similarity of full profiles
    multiply_bipartite(B, mode = "columns", measure = "cosine",
                       threshold = threshold)
  }
}
