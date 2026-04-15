#' Compute local citation scores
#'
#' Counts how many times each document is cited by other documents
#' within the dataset.
#'
#' @param data A data frame with `id` and `references` (list-column).
#'   Optionally `year`, `title`, `journal`, `doi`, `cited_by_count`.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{`id`}{Document identifier.}
#'     \item{`lcs`}{Local Citation Score: times cited within the dataset.}
#'     \item{`gcs`}{Global Citation Score: `cited_by_count` if available.}
#'   }
#'   Plus any metadata columns present in the input (`title`, `year`,
#'   `journal`, `doi`).
#'
#' @export
#' @examples
#' data(biblio_data)
#' local_citations(biblio_data)
local_citations <- function(data) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "references" %in% names(data)
  )

  ids <- as.character(data[["id"]])

  ## Expand all references
  all_refs <- unlist(data[["references"]], use.names = FALSE)
  all_refs <- all_refs[!is.na(all_refs)]

  ## Count how many times each dataset ID appears as a reference
  internal_refs <- all_refs[all_refs %in% ids]
  lcs_table <- table(internal_refs)

  lcs <- integer(length(ids))
  names(lcs) <- ids
  matched <- intersect(names(lcs_table), ids)
  lcs[matched] <- as.integer(lcs_table[matched])

  ## Build result
  result <- data.frame(
    id = ids,
    lcs = as.integer(lcs),
    stringsAsFactors = FALSE
  )

  ## Add GCS if available
  if ("cited_by_count" %in% names(data)) {
    result$gcs <- as.integer(data[["cited_by_count"]])
  }

  ## Add metadata columns if present
  for (col in c("title", "year", "journal", "doi")) {
    if (col %in% names(data)) result[[col]] <- data[[col]]
  }

  result <- result[order(-result$lcs), ]
  rownames(result) <- NULL
  result
}


#' Build a historiograph (chronological citation network)
#'
#' Constructs a Garfield-style historiograph: a directed citation network
#' among the most locally cited documents, laid out chronologically.
#'
#' @param data A data frame with `id`, `references` (list-column), and
#'   `year`. Optionally `title`, `journal`, `doi`, `cited_by_count`.
#' @param n Integer. Number of top locally cited documents to include.
#'   Default 30.
#' @param min_lcs Integer. Minimum local citation score for inclusion.
#'   Default 1.
#'
#' @return A list with:
#'   \describe{
#'     \item{`$nodes`}{Data frame of included documents with `id`, `lcs`,
#'       `gcs`, `year`, `title`, `journal`, `doi`.}
#'     \item{`$edges`}{Data frame of directed citation edges with `from`
#'       (citing), `to` (cited), `year_from`, `year_to`.}
#'   }
#'
#' @export
#' @examples
#' data(biblio_data)
#' h <- historiograph(biblio_data, n = 5)
#' h$nodes
#' h$edges
historiograph <- function(data, n = 30, min_lcs = 1) {
  stopifnot(
    is.data.frame(data),
    "id" %in% names(data),
    "references" %in% names(data),
    "year" %in% names(data)
  )

  ## Compute local citations
  lcs_df <- local_citations(data)

  ## Filter by min_lcs
  lcs_df <- lcs_df[lcs_df$lcs >= min_lcs, ]

  if (nrow(lcs_df) == 0) {
    return(list(
      nodes = data.frame(id = character(0), lcs = integer(0),
                          year = integer(0), stringsAsFactors = FALSE),
      edges = data.frame(from = character(0), to = character(0),
                          year_from = integer(0), year_to = integer(0),
                          stringsAsFactors = FALSE)
    ))
  }

  ## Top n by LCS
  if (nrow(lcs_df) > n) {
    lcs_df <- lcs_df[seq_len(n), ]
  }

  nodes <- lcs_df
  node_ids <- nodes$id

  ## Build directed citation edges among included nodes
  ids <- as.character(data[["id"]])
  refs_list <- data[["references"]]
  years <- data[["year"]]

  ## Year lookup
  year_lookup <- stats::setNames(years, ids)

  ## Expand references for ALL papers (a non-top paper can cite a top paper)
  citing <- rep(ids, lengths(refs_list))
  cited <- unlist(refs_list, use.names = FALSE)

  ## Keep edges where cited is in node_ids
  keep <- !is.na(cited) & cited %in% node_ids
  citing <- citing[keep]
  cited <- cited[keep]

  ## Also keep only if citing is in the dataset
  keep <- citing %in% ids
  citing <- citing[keep]
  cited <- cited[keep]

  if (length(citing) == 0) {
    edges <- data.frame(from = character(0), to = character(0),
                         year_from = integer(0), year_to = integer(0),
                         stringsAsFactors = FALSE)
  } else {
    edges <- data.frame(
      from = citing,
      to = cited,
      year_from = as.integer(year_lookup[citing]),
      year_to = as.integer(year_lookup[cited]),
      stringsAsFactors = FALSE
    )
    ## Remove duplicates
    edges <- unique(edges)
    edges <- edges[order(edges$year_from, edges$year_to), ]
    rownames(edges) <- NULL
  }

  list(nodes = nodes, edges = edges)
}
