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
#' @param counting Character. Counting method. Position-independent methods
#'   (`"full"`, `"fractional"`, `"paper"`, `"strength"`) work for all types.
#'   Position-dependent methods (`"harmonic"`, `"arithmetic"`, `"geometric"`,
#'   `"adaptive_geometric"`, `"golden"`, `"first"`, `"last"`,
#'   `"first_last"`, `"position_weighted"`) are available for
#'   `type = "collaboration"`.
#' @param similarity Character. Similarity measure: `"none"`, `"association"`,
#'   `"cosine"`, `"jaccard"`, `"inclusion"`, `"equivalence"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum number of papers for an author to be
#'   included. Default 1.
#' @param position_weights Numeric vector. Custom weights for
#'   `counting = "position_weighted"`. Default `c(1, 0.8, 0.6, 0.4)`.
#' @param first_last_weight Numeric. Multiplier for `counting = "first_last"`.
#'   Default 2.
#' @param attention Character or NULL. Attention-based weighting independent of
#'   `type` and `counting`. One of `"proximity"` (center authors weighted most),
#'   `"lead"` (first author dominates, quadratic drop), `"last"` (last author
#'   dominates, quadratic rise), `"circular"` (first and last both prominent).
#'   Default `NULL` (disabled).
#' @param top_n Integer or NULL. Return only the top n edges by weight.
#'   Default NULL (all edges).
#' @param self_loops Logical. If `TRUE`, include self-loops (an entity linked
#'   to itself). Default `FALSE`.
#' @param deduplicate Logical. If `TRUE` (default), each `(paper, entity)`
#'   pair is counted at most once — duplicate entries in the source data
#'   (e.g., the same author listed twice on a paper) are treated as one
#'   occurrence. Set to `FALSE` to count every raw occurrence.
#' @param format Character. Output format:
#'   \describe{
#'     \item{`"edgelist"`}{Default. A `bibnets_network` data frame with
#'       columns `from`, `to`, `weight`, `count`.}
#'     \item{`"gephi"`}{Gephi-ready data frame: `Source`, `Target`,
#'       `Weight`, `Count`, `Type`.}
#'     \item{`"igraph"`}{An igraph graph object (requires igraph).}
#'     \item{`"cograph"`}{A cograph_network object (requires cograph).}
#'     \item{`"matrix"`}{A sparse adjacency matrix.}
#'   }
#'
#' @return Depends on `format`: a `bibnets_network` data frame (default),
#'   a Gephi-ready data frame, an igraph graph, a cograph_network, or a
#'   sparse matrix.
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
#' author_network(biblio_data, "collaboration", counting = "harmonic")
#' author_network(biblio_data, "collaboration", counting = "geometric",
#'                similarity = "association")
author_network <- function(data,
                           type = "collaboration",
                           counting = "full",
                           similarity = "none",
                           threshold = 0,
                           min_occur = 1L,
                           position_weights = c(1, 0.8, 0.6, 0.4),
                           first_last_weight = 2,
                           attention = NULL,
                           top_n = NULL,
                           self_loops = FALSE,
                           deduplicate = TRUE,
                           format = "edgelist") {
  check_data(data, c("id", "authors"))
  check_choice(similarity, c("none", "association", "cosine", "jaccard",
                              "inclusion", "equivalence"), "similarity")
  check_format(format)

  if (!is.null(attention)) {
    check_choice(attention, all_attention_methods(), "attention")
    B <- build_author_bipartite(data, field = "authors",
                                counting = paste0("attention_", attention),
                                deduplicate = deduplicate)
    result <- multiply_bipartite(B, mode = "columns", similarity = similarity,
                                 threshold = threshold, top_n = top_n,
                                 self_loops = self_loops)
    return(as_bibnets_network(result,
      network_type = paste0("author_attention_", attention),
      counting = attention, similarity = similarity, format = format))
  }

  check_choice(type, c("collaboration", "coupling", "co_citation", "equivalence"), "type")
  check_choice(counting, all_counts(), "counting")

  is_positional <- counting %in% position_dependent_counts()

  result <- if (type == "collaboration") {
    if (is_positional) {
      B <- build_author_bipartite(
        data, counting = counting,
        position_weights = position_weights,
        first_last_weight = first_last_weight,
        deduplicate = deduplicate
      )
      m <- if (type == "equivalence") "cosine" else similarity
      multiply_bipartite(B, mode = "columns", similarity = m,
                         threshold = threshold, top_n = top_n,
                         self_loops = self_loops)
    } else {
      B <- build_bipartite(data, field = "authors", min_freq = min_occur, deduplicate = deduplicate)
      B <- apply_counting(B, counting = counting, network_type = "symmetric")
      multiply_bipartite(B, mode = "columns", similarity = similarity,
                         threshold = threshold, top_n = top_n,
                         self_loops = self_loops)
    }

  } else if (type == "coupling") {
    check_data(data, "references")
    agg <- aggregate_by_entity(data, entity_field = "authors",
                                value_field = "references")
    B <- build_bipartite(agg, field = "references")
    ct <- if (is_positional) "full" else counting
    B <- apply_counting(B, counting = ct, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", similarity = similarity,
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)

  } else if (type == "co_citation") {
    field <- if ("cited_first_authors" %in% names(data)) {
      "cited_first_authors"
    } else {
      stop("Column 'cited_first_authors' not found. ",
           "Parse reference strings to extract cited authors first.",
           call. = FALSE)
    }
    B <- build_bipartite(data, field = field, min_freq = min_occur, deduplicate = deduplicate)
    ct <- if (is_positional) "full" else counting
    B <- apply_counting(B, counting = ct, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", similarity = similarity,
                       threshold = threshold, top_n = top_n)

  } else {
    B <- build_bipartite(data, field = "authors", min_freq = min_occur)
    multiply_bipartite(B, mode = "columns", similarity = "cosine",
                       threshold = threshold, top_n = top_n,
                       self_loops = self_loops)
  }

  as_bibnets_network(result,
    network_type = paste0("author_", type),
    counting = counting, similarity = similarity,
    format = format)
}
