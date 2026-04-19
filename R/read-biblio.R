#' Read bibliometric data
#'
#' Universal reader that handles files, folders, format detection, and
#' generic CSV input. Accepts a single file, multiple files, or a directory.
#'
#' @param path Character. Path to a file, a vector of file paths, or a
#'   directory containing export files.
#' @param format Character. File format:
#'   \describe{
#'     \item{`"auto"`}{Default. Auto-detect from file content.}
#'     \item{`"scopus"`}{Scopus CSV.}
#'     \item{`"wos"`}{Web of Science plaintext.}
#'     \item{`"wos_tab"`}{Web of Science tab-delimited.}
#'     \item{`"bibtex"`}{BibTeX .bib file.}
#'     \item{`"ris"`}{RIS file.}
#'     \item{`"dimensions"`}{Dimensions CSV.}
#'     \item{`"lens"`}{Lens.org CSV.}
#'     \item{`"openalex_csv"`}{Flat OpenAlex CSV export (pipe-delimited fields).}
#'     \item{`"generic"`}{Any CSV. Use `id` and `actors` to specify columns.}
#'   }
#' @param id Character. Column name for document identifier. Only used
#'   when `format = "generic"`. Default `NULL` (uses row numbers).
#' @param actors Character vector. Column names to split into list-columns.
#'   Only used when `format = "generic"`.
#' @param sep Character. Delimiter for splitting actor columns. Default `";"`.
#' @param ... Additional arguments passed to the format-specific reader.
#'
#' @return A data frame.
#'
#' @export
#' @examples
#' \dontrun{
#' # Auto-detect single file
#' data <- read_biblio("export.csv")
#'
#' # Read entire folder (merges all files)
#' data <- read_biblio("scopus_exports/")
#'
#' # Multiple files
#' data <- read_biblio(c("scopus1.csv", "scopus2.csv"))
#'
#' # Explicit format
#' data <- read_biblio("file.csv", format = "scopus")
#'
#' # Generic CSV with custom columns
#' data <- read_biblio("my_data.csv", format = "generic",
#'                     id = "doc_id",
#'                     actors = c("Authors", "Keywords"),
#'                     sep = ";")
#' }
read_biblio <- function(path,
                        format = "auto",
                        id = NULL,
                        actors = NULL,
                        sep = ";",
                        ...) {
  ## Collect all file paths
  files <- resolve_paths(path)

  if (length(files) == 0) {
    stop("No files found at: ", paste(path, collapse = ", "), call. = FALSE)
  }

  ## Read each file
  dfs <- lapply(files, function(f) {
    read_single_biblio(f, format = format, id = id, actors = actors,
                        sep = sep, ...)
  })

  ## Combine
  result <- do.call(rbind, dfs)
  rownames(result) <- NULL

  if (length(files) > 1) {
    message(sprintf("Read %d files: %d rows total", length(files), nrow(result)))
  }

  result
}


#' Read a single bibliometric file
#' @keywords internal
read_single_biblio <- function(file, format, id, actors, sep, ...) {
  if (format == "generic") {
    return(read_generic(file, id = id, actors = actors, sep = sep))
  }

  if (format == "auto") {
    format <- detect_format(file)
  }

  switch(format,
    scopus       = read_scopus(file, ...),
    wos          = read_wos(file, ...),
    wos_tab      = read_wos(file, format = "tab", ...),
    bibtex       = read_bibtex(file, ...),
    ris          = read_ris(file, ...),
    dimensions   = read_dimensions(file, ...),
    lens         = read_lens(file, ...),
    openalex_csv = read_openalex_csv(file, ...),
    stop(
      "Could not detect file format for: ", file, "\n\n",
      "Supported file formats:\n",
      "  auto, scopus, wos, wos_tab, bibtex, ris, dimensions, lens,\n",
      "  openalex_csv, generic\n\n",
      "Note: Nested OpenAlex (openalexR::oa_fetch()) and Crossref data\n",
      "must be loaded into R first, then converted with\n",
      "read_openalex() or read_crossref().\n\n",
      "For generic CSV, use: read_biblio(file, format = 'generic', ",
      "actors = c('Authors', 'Keywords'), sep = ';')",
      call. = FALSE
    )
  )
}


#' Read a generic CSV with user-specified columns
#' @keywords internal
read_generic <- function(file, id = NULL, actors = NULL, sep = ";") {
  check_file(file)

  data <- utils::read.csv(file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                           check.names = FALSE)

  ## Set ID column
  if (!is.null(id) && id %in% names(data)) {
    data[["id"]] <- as.character(data[[id]])
  } else if (!"id" %in% names(data)) {
    data[["id"]] <- as.character(seq_len(nrow(data)))
  }

  ## Split actor columns into list-columns
  if (!is.null(actors)) {
    cols <- intersect(actors, names(data))
    data[cols] <- lapply(data[cols], split_field, sep = sep)
  }

  data
}


#' Resolve file paths from a file, vector of files, or directory
#' @keywords internal
resolve_paths <- function(path) {
  unlist(lapply(path, function(p) {
    if (dir.exists(p)) {
      list.files(p, pattern = "\\.(csv|txt|bib|ris|xlsx?)$",
                 full.names = TRUE, ignore.case = TRUE)
    } else if (file.exists(p)) {
      p
    } else {
      character(0)
    }
  }), use.names = FALSE)
}


#' Detect bibliometric file format
#' @param file Path to file.
#' @return Character: format name or `"unknown"`.
#' @keywords internal
detect_format <- function(file) {
  lines <- readLines(file, n = 10, warn = FALSE, encoding = "UTF-8")
  lines <- lines[nchar(trimws(lines)) > 0]
  if (length(lines) == 0) return("unknown")

  first <- trimws(lines[1])

  ## BibTeX: starts with @
  if (grepl("^@", first)) return("bibtex")

  ## RIS: starts with TY  -
  if (grepl("^TY\\s+-", first)) return("ris")

  ## WoS plaintext: starts with FN or PT
  if (grepl("^(FN|PT)\\s", first)) return("wos")

  ## CSV-based: check header line — Dimensions prepends a metadata row so
  ## also check line 2 when line 1 looks like "About the data: ..."
  header <- tolower(first)
  if (grepl("^\"?about the data", header) && length(lines) >= 2) {
    header <- tolower(trimws(lines[2]))
  }

  ## Scopus: has EID column
  if (grepl("\\beid\\b", header)) return("scopus")

  ## Dimensions: has "publication id" or "dimensions url"
  if (grepl("publication id|dimensions url", header)) return("dimensions")

  ## Lens: has "lens id"
  if (grepl("lens id", header)) return("lens")

  ## OpenAlex flat CSV: has "authorships.author.display_name" column header
  if (grepl("authorships\\.author\\.display_name", header)) return("openalex_csv")

  "unknown"
}
