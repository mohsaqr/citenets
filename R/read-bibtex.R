#' Read a BibTeX file
#'
#' Parses a `.bib` file into a standardized bibliometric data frame.
#' Note: standard BibTeX does not contain cited references, so the
#' `references` column will be empty unless the file includes a
#' non-standard `cited-references` or `note` field with reference data.
#'
#' @param file Path to a `.bib` file.
#'
#' @return A data frame with the same column structure as [read_scopus()].
#'   The `references` column will typically be empty for standard BibTeX.
#'
#' @export
#' @examples
#' \dontrun{
#' data <- read_bibtex("references.bib")
#' }
read_bibtex <- function(file) {
  stopifnot(file.exists(file))

  lines <- readLines(file, encoding = "UTF-8", warn = FALSE)

  ## Parse BibTeX entries
  entries <- list()
  current <- NULL
  brace_depth <- 0
  field_name <- ""
  field_value <- ""

  for (line in lines) {
    ## Start of new entry
    if (grepl("^@\\w+\\{", line)) {
      if (!is.null(current) && length(current) > 0) {
        entries <- c(entries, list(current))
      }
      ## Extract entry type and key
      m <- regmatches(line, regexec("^@(\\w+)\\{\\s*([^,]*)", line))[[1]]
      current <- list(.type = tolower(m[2]), .key = trimws(m[3]))
      brace_depth <- nchar(gsub("[^{]", "", line)) -
        nchar(gsub("[^}]", "", line))
      ## Check for inline fields
      rest <- sub("^@\\w+\\{[^,]*,?", "", line)
      if (nchar(trimws(rest)) > 0) line <- rest else next
    }

    if (is.null(current)) next

    ## Parse field = {value} or field = "value"
    if (grepl("^\\s*\\w+\\s*=", line)) {
      ## New field
      m <- regmatches(line, regexec("^\\s*(\\w+)\\s*=\\s*(.*)", line))[[1]]
      field_name <- tolower(trimws(m[2]))
      raw_val <- m[3]

      ## Strip braces/quotes
      val <- clean_bibtex_value(raw_val)

      if (grepl("[{}]", raw_val)) {
        open <- nchar(gsub("[^{]", "", raw_val))
        close <- nchar(gsub("[^}]", "", raw_val))
        if (open > close) {
          field_value <- val
          next  # continuation on next lines
        }
      }

      current[[field_name]] <- val
      field_name <- ""

    } else if (nchar(field_name) > 0) {
      ## Continuation of previous field
      val <- clean_bibtex_value(line)
      field_value <- paste(field_value, val)

      open <- nchar(gsub("[^{]", "", line))
      close <- nchar(gsub("[^}]", "", line))
      brace_depth <- brace_depth + open - close

      if (brace_depth <= 1 || grepl("},?\\s*$", line)) {
        current[[field_name]] <- trimws(field_value)
        field_name <- ""
        field_value <- ""
      }
    }
  }

  ## Don't forget last entry

  if (!is.null(current) && length(current) > 0) {
    entries <- c(entries, list(current))
  }

  n <- length(entries)
  if (n == 0) return(empty_biblio_df())

  get_bib <- function(entry, field, default = NA_character_) {
    val <- entry[[field]]
    if (is.null(val) || nchar(trimws(val)) == 0) default else trimws(val)
  }

  result <- data.frame(
    id = vapply(entries, function(e) {
      key <- e[[".key"]]
      if (is.null(key) || nchar(key) == 0) NA_character_ else key
    }, character(1)),
    title = vapply(entries, function(e) get_bib(e, "title"), character(1)),
    year = vapply(entries, function(e) {
      y <- get_bib(e, "year", NA_character_)
      if (is.na(y)) NA_integer_ else as.integer(y)
    }, integer(1)),
    journal = vapply(entries, function(e) {
      get_bib(e, "journal", get_bib(e, "booktitle"))
    }, character(1)),
    doi = vapply(entries, function(e) get_bib(e, "doi"), character(1)),
    cited_by_count = NA_integer_,
    abstract = vapply(entries, function(e) get_bib(e, "abstract"), character(1)),
    type = vapply(entries, function(e) {
      get_bib(e, ".type", NA_character_)
    }, character(1)),
    stringsAsFactors = FALSE
  )

  ## Authors: "LastA, FirstA and LastB, FirstB"
  result$authors <- lapply(entries, function(e) {
    au <- get_bib(e, "author")
    if (is.na(au)) return(character(0))
    parts <- strsplit(au, "\\s+and\\s+", perl = TRUE)[[1]]
    standardize_authors(parts)
  })

  ## Keywords
  result$keywords <- lapply(entries, function(e) {
    kw <- get_bib(e, "keywords")
    if (is.na(kw)) return(character(0))
    parts <- strsplit(kw, "[;,]")[[1]]
    trimws(parts)
  })

  ## References: not standard in BibTeX
  result$references <- replicate(n, character(0), simplify = FALSE)

  result
}


#' @keywords internal
clean_bibtex_value <- function(x) {
  x <- trimws(x)
  x <- gsub("^[{\"]+|[}\",]+$", "", x)
  x <- gsub("\\{|\\}", "", x)
  trimws(x)
}
