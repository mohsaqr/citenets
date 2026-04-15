#' Build time-windowed networks
#'
#' Splits data by time windows and builds a separate network for each
#' window using any network function.
#'
#' @param data A data frame with a `year` column.
#' @param network_fun Function or character string naming a network function
#'   (e.g., `author_network`, `"reference_network"`, `conetwork`).
#' @param ... Additional arguments passed to `network_fun` (e.g., `type`,
#'   `counting`, `similarity`, `threshold`, `top_n`).
#' @param window Integer. Width of each time window in years. Default 3.
#' @param step Integer or `NULL`. Step size between windows. Default `NULL`
#'   (equals `window` for fixed, 1 for sliding).
#' @param strategy Character. Time window strategy:
#'   \describe{
#'     \item{`"fixed"`}{Disjoint non-overlapping windows (default).}
#'     \item{`"sliding"`}{Overlapping windows advancing by `step` years.}
#'     \item{`"cumulative"`}{Each window starts at the earliest year and
#'       extends further.}
#'   }
#'
#' @return A named list of data frames (edge lists). Names are window
#'   labels like `"2018-2020"`.
#'
#' @export
#' @examples
#' data(biblio_data)
#'
#' # Fixed 3-year windows
#' temporal_network(biblio_data, author_network, "collaboration")
#'
#' # Sliding window
#' temporal_network(biblio_data, author_network, "collaboration",
#'                  window = 2, strategy = "sliding")
#'
#' # Cumulative
#' temporal_network(biblio_data, reference_network,
#'                  threshold = 0, strategy = "cumulative", window = 2)
#'
#' # With string name
#' temporal_network(biblio_data, "keyword_network", window = 3)
temporal_network <- function(data,
                             network_fun,
                             ...,
                             window = 3,
                             step = NULL,
                             strategy = "fixed") {
  stopifnot(
    is.data.frame(data),
    "year" %in% names(data),
    strategy %in% c("fixed", "sliding", "cumulative"),
    is.numeric(window), window >= 1
  )

  if (is.character(network_fun)) {
    network_fun <- match.fun(network_fun)
  }
  stopifnot(is.function(network_fun))

  years <- data[["year"]]
  years <- years[!is.na(years)]
  if (length(years) == 0) return(list())

  min_year <- min(years)
  max_year <- max(years)

  windows <- build_windows(min_year, max_year, window, step, strategy)

  window_list <- lapply(seq_len(nrow(windows)), function(i) {
    subset_data <- data[!is.na(data$year) &
                          data$year >= windows$start[i] &
                          data$year <= windows$end[i], ]
    if (nrow(subset_data) < 2) return(NULL)
    edges <- tryCatch(network_fun(subset_data, ...), error = function(e) NULL)
    if (!is.null(edges) && nrow(edges) > 0) {
      edges$window <- windows$label[i]
      edges
    } else NULL
  })

  Filter(Negate(is.null), stats::setNames(window_list, windows$label))
}


#' Build time window definitions
#' @keywords internal
build_windows <- function(min_year, max_year, window, step, strategy) {
  window <- as.integer(window)

  if (strategy == "fixed") {
    if (is.null(step)) step <- window
    starts <- seq(min_year, max_year, by = step)
    ends <- pmin(starts + window - 1L, max_year)

  } else if (strategy == "sliding") {
    if (is.null(step)) step <- 1L
    starts <- seq(min_year, max_year - window + 1L, by = step)
    ends <- starts + window - 1L

  } else if (strategy == "cumulative") {
    if (is.null(step)) step <- 1L
    ends <- seq(min_year + window - 1L, max_year, by = step)
    starts <- rep(min_year, length(ends))
  }

  labels <- paste0(starts, "-", ends)

  data.frame(start = starts, end = ends, label = labels,
             stringsAsFactors = FALSE)
}
