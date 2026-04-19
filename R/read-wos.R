#' Read Web of Science plaintext or tab-delimited export
#'
#' Parses a Web of Science export file (plaintext or tab-delimited) into a
#' standardized bibliometric data frame.
#'
#' @param file Path to a WoS export file (.txt).
#' @param format Character. `"plaintext"` (default) for WoS tagged format,
#'   or `"tab"` for tab-delimited export.
#'
#' @return A data frame in the standard bibnets format: `id`, `title`,
#'   `year`, `journal`, `doi`, `cited_by_count`, `abstract`, `type`,
#'   plus list-columns `authors`, `references`, and `keywords`.
#'   WoS-specific extra: `keywords_plus` (list-column).
#'
#' @export
#' @examples
#' \dontrun{
#' data <- read_wos("savedrecs.txt")
#' }
read_wos <- function(file, format = "plaintext") {
  check_file(file)
  check_choice(format, c("plaintext", "tab"), "format")

  if (format == "tab") {
    return(read_wos_tab(file))
  }

  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)

  ## Parse tagged format: each record starts with field tags
  ## Records are delimited by "ER" lines
  records <- list()
  current <- list()
  current_tag <- ""

  for (line in lines) {
    if (grepl("^ER\\s*$", line)) {
      if (length(current) > 0) {
        records <- c(records, list(current))
      }
      current <- list()
      current_tag <- ""
      next
    }

    ## Skip file headers
    if (grepl("^(FN |VR |EF\\s*$)", line)) next

    ## Tag line: first two characters are the tag
    tag <- substr(line, 1, 2)
    value <- trimws(substr(line, 4, nchar(line)))

    if (tag != "  ") {
      current_tag <- tag
      current[[current_tag]] <- c(current[[current_tag]], value)
    } else if (nchar(current_tag) > 0) {
      ## Continuation line
      current[[current_tag]] <- c(current[[current_tag]], value)
    }
  }

  ## Convert records to data frame
  n <- length(records)
  if (n == 0) {
    return(empty_biblio_df())
  }

  get_field <- function(rec, tag, collapse = NULL) {
    val <- rec[[tag]]
    if (is.null(val)) return(NA_character_)
    if (!is.null(collapse)) paste(val, collapse = collapse) else val[1]
  }

  id <- vapply(records, function(r) {
    ut <- get_field(r, "UT")
    if (is.na(ut)) paste0("WOS", which(vapply(records, identical, logical(1), r)))
    else ut
  }, character(1))

  title <- vapply(records, function(r) get_field(r, "TI", collapse = " "),
                  character(1))

  authors_raw <- lapply(records, function(r) {
    au <- r[["AU"]]
    if (is.null(au)) character(0) else standardize_authors(au)
  })

  year <- vapply(records, function(r) {
    py <- get_field(r, "PY")
    if (is.na(py)) NA_integer_ else as.integer(py)
  }, integer(1))

  journal <- vapply(records, function(r) get_field(r, "SO"), character(1))

  doi <- vapply(records, function(r) get_field(r, "DI"), character(1))

  refs_raw <- lapply(records, function(r) {
    cr <- r[["CR"]]
    if (is.null(cr)) return(character(0))
    ## CR field may have multiple refs per line, semicolon-delimited
    refs <- unlist(strsplit(cr, ";"), use.names = FALSE)
    standardize_refs(refs)
  })

  keywords <- lapply(records, function(r) {
    de <- r[["DE"]]
    if (is.null(de)) return(character(0))
    kw <- unlist(strsplit(paste(de, collapse = ";"), ";"), use.names = FALSE)
    trimws(kw)
  })

  keywords_plus <- lapply(records, function(r) {
    id_field <- r[["ID"]]
    if (is.null(id_field)) return(character(0))
    kw <- unlist(strsplit(paste(id_field, collapse = ";"), ";"),
                  use.names = FALSE)
    trimws(kw)
  })

  cited_by <- vapply(records, function(r) {
    tc <- get_field(r, "TC")
    if (is.na(tc)) 0L else as.integer(tc)
  }, integer(1))

  abstract <- vapply(records, function(r) get_field(r, "AB", collapse = " "),
                     character(1))

  type <- vapply(records, function(r) get_field(r, "DT"), character(1))

  result <- data.frame(
    id = id,
    title = title,
    year = year,
    journal = journal,
    doi = doi,
    cited_by_count = cited_by,
    abstract = abstract,
    type = type,
    stringsAsFactors = FALSE
  )

  result$authors <- authors_raw
  result$references <- refs_raw
  result$keywords <- keywords
  result$keywords_plus <- keywords_plus

  result
}


#' @keywords internal
read_wos_tab <- function(file) {
  raw <- utils::read.delim(file, stringsAsFactors = FALSE, quote = "",
                            fileEncoding = "UTF-8", check.names = FALSE)

  get_col <- function(names_vec, default = NA_character_) {
    for (nm in names_vec) {
      if (nm %in% names(raw)) return(raw[[nm]])
    }
    rep(default, nrow(raw))
  }

  id <- get_col(c("UT", "Accession Number"), paste0("WOS", seq_len(nrow(raw))))

  result <- data.frame(
    id = id,
    title = get_col(c("TI", "Title")),
    year = as.integer(get_col(c("PY", "Publication Year"), NA_integer_)),
    journal = get_col(c("SO", "Source Title")),
    doi = get_col(c("DI", "DOI")),
    cited_by_count = as.integer(get_col(c("TC", "Times Cited"), 0L)),
    abstract = get_col(c("AB", "Abstract")),
    type = get_col(c("DT", "Document Type")),
    stringsAsFactors = FALSE
  )

  result$authors <- split_field(get_col(c("AU", "Authors")), sep = ";")
  result$authors <- lapply(result$authors, standardize_authors)

  result$references <- split_field(get_col(c("CR", "Cited References")),
                                    sep = ";")
  result$references <- lapply(result$references, standardize_refs)

  result$keywords <- split_field(get_col(c("DE", "Author Keywords")), sep = ";")
  result$keywords <- lapply(result$keywords, trimws)

  result$keywords_plus <- split_field(get_col(c("ID", "Keywords Plus")),
                                       sep = ";")
  result$keywords_plus <- lapply(result$keywords_plus, trimws)

  result
}


#' @keywords internal
empty_biblio_df <- function() {
  result <- data.frame(
    id = character(0),
    title = character(0),
    year = integer(0),
    journal = character(0),
    doi = character(0),
    cited_by_count = integer(0),
    abstract = character(0),
    type = character(0),
    stringsAsFactors = FALSE
  )
  result$authors <- list()
  result$references <- list()
  result$keywords <- list()
  result
}
