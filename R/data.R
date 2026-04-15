#' Example bibliometric dataset
#'
#' A small synthetic dataset of 10 scholarly papers with overlapping authors,
#' references, and keywords. Designed for testing and demonstrating all
#' network construction functions in citenets.
#'
#' @format A data frame with 10 rows and 9 columns:
#' \describe{
#'   \item{id}{Unique document identifier (W1--W10).}
#'   \item{title}{Document title.}
#'   \item{year}{Publication year (2018--2022).}
#'   \item{journal}{Source journal (Scientometrics, Journal of Informetrics,
#'     JASIST, Quantitative Science Studies).}
#'   \item{doi}{DOI string.}
#'   \item{cited_by_count}{Times cited.}
#'   \item{authors}{List-column of author name strings (6 unique authors).}
#'   \item{references}{List-column of cited reference IDs (10 unique refs,
#'     R1--R10). Each paper cites exactly 4 references.}
#'   \item{keywords}{List-column of keyword strings (24 unique keywords).
#'     Each paper has 3 keywords.}
#' }
#'
#' @examples
#' data(biblio_data)
#' co_citation(biblio_data)
#' coupling(biblio_data)
#' co_authorship(biblio_data)
"biblio_data"
