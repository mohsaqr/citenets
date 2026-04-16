#' Convert edge data frame to igraph
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns,
#'   as returned by any network function in citenets.
#' @param directed Logical. Default `FALSE`.
#'
#' @return An igraph graph object.
#'
#' @export
#' @examples
#' \dontrun{
#' data(biblio_data)
#' edges <- author_network(biblio_data, "collaboration")
#' g <- to_igraph(edges)
#' }
to_igraph <- function(edges, directed = FALSE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required. Install it with: ",
         "install.packages('igraph')", call. = FALSE)
  }
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )
  igraph::graph_from_data_frame(edges, directed = directed)
}


#' Convert edge data frame to tbl_graph
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns.
#' @param directed Logical. Default `FALSE`.
#'
#' @return A tbl_graph object (tidygraph).
#'
#' @export
#' @examples
#' \dontrun{
#' data(biblio_data)
#' edges <- keyword_network(biblio_data)
#' tg <- to_tbl_graph(edges)
#' }
to_tbl_graph <- function(edges, directed = FALSE) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' is required. Install it with: ",
         "install.packages('tidygraph')", call. = FALSE)
  }
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )
  tidygraph::as_tbl_graph(to_igraph(edges, directed = directed))
}


#' Convert edge data frame to adjacency matrix
#'
#' @param edges A data frame with `from`, `to`, `weight` columns.
#' @param symmetric Logical. If `TRUE` (default), produces a symmetric matrix.
#'
#' @return A sparse Matrix.
#'
#' @export
#' @examples
#' data(biblio_data)
#' edges <- reference_network(biblio_data, min_occur = 2)
#' to_matrix(edges)
to_matrix <- function(edges, symmetric = TRUE) {
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )
  edgelist_to_mat(edges, symmetric = symmetric)
}


#' Export to Gephi node and edge tables
#'
#' Converts a citenets edge list (and optional node table) to the CSV format
#' expected by Gephi's Data Laboratory. Column names are remapped to Gephi
#' conventions (`Source`, `Target`, `Weight`, `Id`, `Label`).
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns.
#' @param nodes Optional data frame of node attributes. Must contain an `id`
#'   column. All other columns are included as Gephi node attributes.
#' @param file Optional directory path. If supplied, writes `nodes.csv` and
#'   `edges.csv` into that directory. If `NULL` (default), returns a list.
#' @param directed Logical. Sets the `Type` column. Default `FALSE`.
#'
#' @return If `file = NULL`: a list with `$nodes` and `$edges` data frames.
#'   If `file` is a directory path: writes two CSV files invisibly and returns
#'   the file paths.
#'
#' @export
#' @examples
#' data(biblio_data)
#' edges <- author_network(biblio_data, "collaboration")
#' gephi <- to_gephi(edges)
#' head(gephi$edges)
to_gephi <- function(edges, nodes = NULL, file = NULL, directed = FALSE) {
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )

  type_label <- if (directed) "Directed" else "Undirected"

  ## Edge table — Gephi expects Source, Target, Weight, Type
  edge_out <- edges
  names(edge_out)[names(edge_out) == "from"]   <- "Source"
  names(edge_out)[names(edge_out) == "to"]     <- "Target"
  names(edge_out)[names(edge_out) == "weight"] <- "Weight"
  edge_out$Type <- type_label

  ## Move standard columns to the front
  first <- intersect(c("Source", "Target", "Weight", "Type"), names(edge_out))
  edge_out <- edge_out[, c(first, setdiff(names(edge_out), first))]

  ## Node table — derive from edge list if not supplied
  if (is.null(nodes)) {
    all_ids <- unique(c(edges$from, edges$to))
    node_out <- data.frame(Id = all_ids, Label = all_ids,
                           stringsAsFactors = FALSE)
  } else {
    stopifnot(is.data.frame(nodes), "id" %in% names(nodes))
    node_out <- nodes
    names(node_out)[names(node_out) == "id"] <- "Id"
    node_out$Label <- node_out$Id
    first_n <- intersect(c("Id", "Label"), names(node_out))
    node_out <- node_out[, c(first_n, setdiff(names(node_out), first_n))]
  }

  if (is.null(file)) {
    return(list(nodes = node_out, edges = edge_out))
  }

  stopifnot(dir.exists(file))
  node_path <- file.path(file, "nodes.csv")
  edge_path <- file.path(file, "edges.csv")
  utils::write.csv(node_out, node_path, row.names = FALSE)
  utils::write.csv(edge_out, edge_path, row.names = FALSE)
  message("Written: ", node_path, "\n        ", edge_path)
  invisible(c(node_path, edge_path))
}


#' Export to GraphML
#'
#' Writes a citenets edge list (and optional node attributes) to a GraphML
#' file using pure base R — no XML package required.
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns.
#' @param nodes Optional data frame of node attributes with an `id` column.
#' @param file File path to write. If `NULL` (default), returns the GraphML
#'   as a character string.
#' @param directed Logical. Default `FALSE`.
#'
#' @return If `file = NULL`: GraphML as a character string. Otherwise writes
#'   the file and returns the path invisibly.
#'
#' @export
#' @examples
#' data(biblio_data)
#' edges <- keyword_network(biblio_data)
#' xml <- to_graphml(edges)
#' cat(substr(xml, 1, 300))
to_graphml <- function(edges, nodes = NULL, file = NULL, directed = FALSE) {
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )

  r_to_graphml_type <- function(x) {
    if (is.integer(x))   return("int")
    if (is.numeric(x))   return("double")
    if (is.logical(x))   return("boolean")
    "string"
  }

  xml_escape <- function(x) {
    x <- gsub("&",  "&amp;",  as.character(x))
    x <- gsub("<",  "&lt;",   x)
    x <- gsub(">",  "&gt;",   x)
    x <- gsub("\"", "&quot;", x)
    x
  }

  edge_default <- if (directed) "directed" else "undirected"

  ## Key declarations
  edge_attr_cols <- setdiff(names(edges), c("from", "to"))
  edge_keys <- vapply(edge_attr_cols, function(col) {
    sprintf('  <key id="%s" for="edge" attr.name="%s" attr.type="%s"/>',
            col, col, r_to_graphml_type(edges[[col]]))
  }, character(1L))

  node_attr_cols <- character(0)
  node_keys <- character(0)
  if (!is.null(nodes)) {
    stopifnot(is.data.frame(nodes), "id" %in% names(nodes))
    node_attr_cols <- setdiff(names(nodes), "id")
    node_keys <- vapply(node_attr_cols, function(col) {
      sprintf('  <key id="%s" for="node" attr.name="%s" attr.type="%s"/>',
              col, col, r_to_graphml_type(nodes[[col]]))
    }, character(1L))
  }

  ## Node elements
  all_ids <- unique(c(edges$from, edges$to))
  node_elems <- vapply(all_ids, function(v) {
    attrs <- ""
    if (!is.null(nodes)) {
      row <- nodes[nodes$id == v, , drop = FALSE]
      if (nrow(row) > 0) {
        attrs <- paste(vapply(node_attr_cols, function(col) {
          sprintf('      <data key="%s">%s</data>', col,
                  xml_escape(row[[col]][1]))
        }, character(1L)), collapse = "\n")
        attrs <- paste0("\n", attrs, "\n    ")
      }
    }
    sprintf('    <node id="%s">%s</node>', xml_escape(v), attrs)
  }, character(1L))

  ## Edge elements
  edge_elems <- vapply(seq_len(nrow(edges)), function(i) {
    data_tags <- paste(vapply(edge_attr_cols, function(col) {
      sprintf('      <data key="%s">%s</data>', col,
              xml_escape(edges[[col]][i]))
    }, character(1L)), collapse = "\n")
    sprintf('    <edge source="%s" target="%s">\n%s\n    </edge>',
            xml_escape(edges$from[i]), xml_escape(edges$to[i]), data_tags)
  }, character(1L))

  ## Assemble
  lines <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<graphml xmlns="http://graphml.graphdrawing.org/graphml"',
    '         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
    '         xsi:schemaLocation="http://graphml.graphdrawing.org/graphml',
    '         http://graphml.graphdrawing.org/graphml/1.0/graphml.xsd">',
    node_keys,
    edge_keys,
    sprintf('  <graph id="G" edgedefault="%s">', edge_default),
    node_elems,
    edge_elems,
    '  </graph>',
    '</graphml>'
  )

  xml <- paste(lines, collapse = "\n")

  if (is.null(file)) return(xml)

  writeLines(xml, file, useBytes = FALSE)
  message("Written: ", file)
  invisible(file)
}


#' Prepare network for cograph::splot()
#'
#' Converts a citenets edge list to a `cograph_network` object by calling
#' `cograph::as_cograph()`. Optionally merges node metadata (e.g., from
#' [local_citations()]) into the network's node table so attributes like
#' `lcs` or `year` can be used directly in `splot()` aesthetic parameters
#' (e.g., `node_size = "lcs"`).
#'
#' Note: citenets edge lists (`from`, `to`, `weight`) are accepted directly
#' by `cograph::splot()` without conversion. This function is only needed
#' when you want to attach node-level metadata.
#'
#' @param edges A data frame with at least `from`, `to`, `weight` columns.
#' @param nodes Optional data frame of node attributes with an `id` column
#'   (e.g., output of [local_citations()]). All columns are merged into the
#'   `cograph_network$nodes` table and become available as aesthetic mappings.
#' @param directed Logical. Default `FALSE`.
#'
#' @return A `cograph_network` object (S3 list with `$nodes` and `$edges`).
#'
#' @export
#' @examples
#' \dontrun{
#' data(biblio_data)
#'
#' # Without metadata: splot() accepts citenets edges directly
#' edges <- author_network(biblio_data, "collaboration")
#' cograph::splot(edges)
#'
#' # With metadata: document network + local citation scores as node size
#' edges <- document_network(biblio_data, type = "coupling")
#' nodes <- local_citations(biblio_data)   # keyed by document id
#' net   <- to_cograph(edges, nodes = nodes)
#' cograph::splot(net, node_size = "lcs", labels = TRUE)
#' }
to_cograph <- function(edges, nodes = NULL, directed = FALSE) {
  if (!requireNamespace("cograph", quietly = TRUE)) {
    stop("Package 'cograph' is required. Install it with: ",
         'install.packages("cograph", repos = "https://mohsaqr.r-universe.dev")',
         call. = FALSE)
  }
  stopifnot(
    is.data.frame(edges),
    all(c("from", "to", "weight") %in% names(edges))
  )

  net <- cograph::as_cograph(edges, directed = directed)

  if (!is.null(nodes)) {
    stopifnot(is.data.frame(nodes), "id" %in% names(nodes))
    ## Match node metadata by label (cograph stores node names in $nodes$label)
    attr_cols <- setdiff(names(nodes), "id")
    idx <- match(net$nodes$label, nodes$id)
    for (col in attr_cols) {
      net$nodes[[col]] <- nodes[[col]][idx]
    }
  }

  net
}
