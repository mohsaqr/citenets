#' Build a bipartite incidence matrix from bibliometric data
#'
#' Constructs a sparse works x entities two-mode matrix from a data frame
#' with a list-column. This is the core engine behind all network construction
#' functions, following the approach of Batagelj & Cerinsek (2013).
#'
#' @param data A data frame with at least columns `id` and the field specified.
#' @param field Character. Name of the list-column containing entities
#'   (e.g., `"authors"`, `"references"`, `"keywords"`).
#' @param min_freq Integer. Minimum number of occurrences for an entity to be
#'   included. Default 1 (no filtering).
#'
#' @return A sparse `dgCMatrix` with rows = works (named by `id`) and
#'   columns = unique entities.
#'
#' @references
#' Batagelj, V., & Cerinsek, M. (2013). On bibliographic networks.
#' *Scientometrics*, 96(3), 845--864. \doi{10.1007/s11192-012-0940-1}
#'
#' @keywords internal
build_bipartite <- function(data, field, min_freq = 1L, deduplicate = TRUE) {
  check_data(data, c("id", field))
  data <- ensure_list_column(data, field)

  ids <- as.character(data[["id"]])
  entities_list <- data[[field]]

  ## Expand to long form: (work_index, entity_name)
  work_idx <- rep(seq_along(ids), lengths(entities_list))
  entity_names <- unlist(entities_list, use.names = FALSE)

  ## Basic normalization: trim whitespace, case-fold to uppercase
  entity_names <- toupper(trimws(entity_names))

  ## Drop NAs, empty strings, whitespace-only
  keep <- !is.na(entity_names) & nchar(entity_names) > 0
  work_idx <- work_idx[keep]
  entity_names <- entity_names[keep]

  ## Frequency filter
  if (min_freq > 1L) {
    freq <- table(entity_names)
    keep_entities <- names(freq)[freq >= min_freq]
    keep <- entity_names %in% keep_entities
    work_idx <- work_idx[keep]
    entity_names <- entity_names[keep]
  }

  ## Optionally deduplicate: each (paper, entity) pair counts at most once
  if (deduplicate) {
    dup <- duplicated(paste(work_idx, entity_names))
    work_idx     <- work_idx[!dup]
    entity_names <- entity_names[!dup]
  }

  ## Map entity names to integer indices
  unique_entities <- sort(unique(entity_names))
  entity_idx <- match(entity_names, unique_entities)

  ## Build sparse matrix
  B <- Matrix::sparseMatrix(
    i = work_idx,
    j = entity_idx,
    x = rep(1, length(work_idx)),
    dims = c(length(ids), length(unique_entities)),
    dimnames = list(ids, unique_entities)
  )

  B
}


#' Build bipartite matrix from a long-format edge table
#'
#' Alternative constructor when data is already in long form (e.g., a two-column
#' data frame of document-reference pairs).
#'
#' @param edges A data frame with columns `source` (work id) and `target`
#'   (entity id).
#' @param min_freq Integer. Minimum entity frequency. Default 1.
#'
#' @return A sparse `dgCMatrix`.
#' @keywords internal
build_bipartite_long <- function(edges, min_freq = 1L) {
  stopifnot(
    is.data.frame(edges),
    all(c("source", "target") %in% names(edges))
  )

  sources <- toupper(trimws(as.character(edges[["source"]])))
  targets <- toupper(trimws(as.character(edges[["target"]])))

  keep <- !is.na(sources) & !is.na(targets) &
    nchar(sources) > 0 & nchar(targets) > 0
  sources <- sources[keep]
  targets <- targets[keep]

  if (min_freq > 1L) {
    freq <- table(targets)
    keep_targets <- names(freq)[freq >= min_freq]
    keep <- targets %in% keep_targets
    sources <- sources[keep]
    targets <- targets[keep]
  }

  unique_sources <- sort(unique(sources))
  unique_targets <- sort(unique(targets))

  i <- match(sources, unique_sources)
  j <- match(targets, unique_targets)

  Matrix::sparseMatrix(
    i = i, j = j,
    x = rep(1, length(i)),
    dims = c(length(unique_sources), length(unique_targets)),
    dimnames = list(unique_sources, unique_targets)
  )
}
