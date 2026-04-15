#' Build a document network
#'
#' Constructs a network between documents (papers) in the dataset.
#'
#' @param data A data frame with `id` and `references` (list-column).
#' @param type Character. Relationship type:
#'   \describe{
#'     \item{`"coupling"`}{Bibliographic coupling: documents linked when they
#'       share cited references.}
#'     \item{`"citation"`}{Direct citation: directed edges from citing to
#'       cited documents (internal citations only).}
#'     \item{`"co_citation"`}{Co-citation: documents linked when they are
#'       cited together by other documents in the dataset.}
#'     \item{`"equivalence"`}{Profile similarity of reference vectors.}
#'   }
#' @param count Character. Counting method. Default `"full"`.
#'   Position-dependent methods are not applicable to document networks.
#' @param measure Character. Similarity measure. Default `"none"`.
#' @param threshold Numeric. Minimum edge weight. Default 0.
#' @param min_occur Integer. Minimum reference frequency. Default 1.
#'
#' @return A data frame with columns `from`, `to`, `weight`, `count`, `shared`.
#'   For `type = "citation"`, the edges are directed (from = citing,
#'   to = cited).
#'
#' @export
#' @examples
#' data(biblio_data)
#' document_network(biblio_data, "coupling")
#' document_network(biblio_data, "coupling", count = "strength")
document_network <- function(data,
                             type = "coupling",
                             count = "full",
                             measure = "none",
                             threshold = 0,
                             min_occur = 1L) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "references" %in% names(data),
    type %in% c("coupling", "citation", "co_citation", "equivalence"),
    count %in% position_independent_counts(),
    measure %in% c("none", "association", "cosine", "jaccard",
                    "inclusion", "equivalence")
  )

  if (type == "citation") {
    return(build_direct_citation(data))
  }

  B <- build_bipartite(data, field = "references", min_freq = min_occur)

  if (type == "coupling") {
    B <- apply_counting(B, count = count, network_type = "coupling")
    multiply_bipartite(B, mode = "rows", measure = measure,
                       threshold = threshold)

  } else if (type == "co_citation") {
    B <- apply_counting(B, count = count, network_type = "symmetric")
    multiply_bipartite(B, mode = "columns", measure = measure,
                       threshold = threshold)

  } else if (type == "equivalence") {
    multiply_bipartite(B, mode = "rows", measure = "cosine",
                       threshold = threshold)
  }
}


#' @keywords internal
build_direct_citation <- function(data) {
  ids <- as.character(data[["id"]])
  refs_list <- data[["references"]]

  citing <- rep(ids, lengths(refs_list))
  cited <- unlist(refs_list, use.names = FALSE)

  keep <- cited %in% ids & !is.na(cited) & citing != cited
  citing <- citing[keep]
  cited <- cited[keep]

  if (length(citing) == 0L) {
    return(data.frame(
      from = character(0), to = character(0),
      weight = numeric(0), count = integer(0), shared = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    from = citing, to = cited,
    weight = rep(1, length(citing)),
    count = rep(1L, length(citing)),
    shared = rep(1L, length(citing)),
    stringsAsFactors = FALSE
  )
}
