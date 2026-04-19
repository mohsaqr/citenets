## Hard stopwords — break phrase boundaries
.abstract_hard_stops <- c(
  "a", "an", "the",
  "and", "or", "but", "nor", "yet", "so",
  "am", "is", "are", "was", "were", "be", "been", "being",
  "do", "does", "did", "have", "has", "had",
  "will", "would", "can", "could", "shall", "should", "may", "might", "must",
  "i", "we", "you", "he", "she", "it", "they", "me", "us", "him", "them",
  "this", "that", "these", "those",
  "my", "our", "your", "his", "her", "its", "their",
  "who", "which", "what", "when", "where", "how", "why",
  "then", "also", "however", "therefore", "thus",
  "while", "although", "since", "because", "if", "whether",
  "such", "each", "all", "some", "any", "few",
  "more", "most", "other", "many", "much",
  "very", "just", "only", "even",
  "not", "no", "neither", "both", "either",
  "here", "there", "up", "down", "out", "over",
  "under", "again", "further", "once", "per",
  "about", "after", "before", "into", "through",
  "during", "along", "across", "upon",
  "et", "al", "vs", "etc", "ie", "eg",
  "well", "already", "never", "always", "often", "recently",
  "new", "different", "various", "several", "certain",
  "respectively", "moreover", "furthermore", "consequently",
  ## Academic boilerplate nouns
  "paper", "article", "findings", "results", "methods", "collected",
  "conclusion", "conclusions", "contribution", "contributions",
  "implications", "limitations", "significance",
  ## Report verbs
  "presents", "present", "proposes", "propose", "examines", "examine",
  "explores", "explore", "investigates", "investigate",
  "discusses", "discuss", "describes", "describe",
  "highlights", "highlight", "identifies", "identify",
  "reveals", "reveal", "indicates", "indicate",
  "demonstrates", "demonstrate", "shows", "show", "shown",
  "suggests", "suggest", "aims", "aim",
  "analyzes", "analyze", "analyzes", "analyse", "analyzes", "analyses",
  "conducted", "performed", "carried", "reported"
)

## Soft stopwords — allowed inside multi-word phrases (not at start/end)
.abstract_soft_stops <- c(
  "of", "in", "for", "on", "with", "to", "by", "from",
  "at", "as", "via",
  "within", "towards", "toward", "between", "among",
  "using", "based", "applied", "related"
)

## Optional pre-processing (used for testing / exploration only — not called
## by default). Strips publisher copyright notices appended by WoS/Scopus.
strip_abstract_noise <- function(text) {
  text <- gsub("\u00a9\\s*\\d{4}[^\\.]*\\.", "", text, perl = TRUE)
  text <- gsub("\\d{4}\\s+[A-Z][A-Za-z\\s]+Ltd[^\\.]*\\.", "", text, perl = TRUE)
  text <- gsub("\\bAll rights reserved\\b[^.]*\\.", "", text,
               ignore.case = TRUE, perl = TRUE)
  text <- gsub("\\brights reserved\\b", "", text, ignore.case = TRUE)
  text <- gsub("\\bPublished by [A-Z][A-Za-z\\s]+\\.", "", text, perl = TRUE)
  text <- gsub("\\([A-Za-z][A-Za-z0-9,\\.\\s]{0,15}\\)", " ", text, perl = TRUE)
  text
}


## ── AttentionRank helpers ─────────────────────────────────────────────────────

## Build raw (unweighted) pair co-occurrence counts across all abstracts.
## Used as input for all cross-attention proxy methods.
raw_cooccurrence_edges <- function(doc_cands, vocab, window) {
  sep <- "\x01"
  pair_list <- lapply(doc_cands, function(dc) {
    dc <- dc[dc$phrase %in% vocab, , drop = FALSE]
    if (nrow(dc) < 2L) return(NULL)
    dc  <- dc[order(dc$position), , drop = FALSE]
    pos <- dc$position
    ph  <- dc$phrase
    n   <- nrow(dc)
    idx <- which(outer(pos, pos, function(a, b) b > a & (b - a) <= window),
                 arr.ind = TRUE)
    if (nrow(idx) == 0L) return(NULL)
    data.frame(
      from = pmin(ph[idx[, 1L]], ph[idx[, 2L]]),
      to   = pmax(ph[idx[, 1L]], ph[idx[, 2L]]),
      stringsAsFactors = FALSE
    )
  })
  pair_list <- pair_list[!vapply(pair_list, is.null, logical(1L))]
  if (length(pair_list) == 0L) return(NULL)
  all_pairs <- do.call(rbind, pair_list)
  key  <- paste(all_pairs$from, all_pairs$to, sep = sep)
  cnt  <- tapply(rep(1L, length(key)), key, sum)
  parts <- strsplit(names(cnt), sep, fixed = TRUE)
  data.frame(
    from  = vapply(parts, `[[`, character(1L), 1L),
    to    = vapply(parts, `[[`, character(1L), 2L),
    count = as.integer(cnt),
    stringsAsFactors = FALSE
  )
}

## Cross-attention proxy 1 — document frequency (how many docs contain the phrase)
cross_by_freq <- function(doc_cands, vocab, n_docs) {
  df_vec <- vapply(vocab, function(p)
    sum(vapply(doc_cands, function(dc) any(dc$phrase == p), logical(1L))),
    integer(1L))
  stats::setNames(as.numeric(df_vec), vocab)
}

## Cross-attention proxy 2 — degree centrality in raw co-occurrence graph
cross_by_degree <- function(raw_edges, vocab) {
  if (is.null(raw_edges))
    return(stats::setNames(rep(0, length(vocab)), vocab))
  deg <- table(c(raw_edges$from, raw_edges$to))
  r_c <- vapply(vocab, function(p) as.numeric(deg[p]), numeric(1L))
  r_c[is.na(r_c)] <- 0
  stats::setNames(r_c, vocab)
}

## Cross-attention proxy 3 — PageRank on raw co-occurrence graph
cross_by_pagerank <- function(raw_edges, vocab, n_iter = 30L, damp = 0.85) {
  n_v   <- length(vocab)
  if (is.null(raw_edges))
    return(stats::setNames(rep(1 / n_v, n_v), vocab))
  v_idx <- stats::setNames(seq_len(n_v), vocab)

  from_i   <- v_idx[raw_edges$from]
  to_i     <- v_idx[raw_edges$to]
  w        <- as.numeric(raw_edges$count)
  from_all <- c(from_i, to_i)   ## symmetric
  to_all   <- c(to_i,   from_i)
  w_all    <- c(w, w)

  out_sums <- tapply(w_all, from_all, sum)
  out_deg  <- rep(0, n_v)
  out_deg[as.integer(names(out_sums))] <- out_sums
  out_deg[out_deg == 0] <- 1

  one_iter <- function(pr, dummy) {
    contrib  <- damp * pr[from_all] * w_all / out_deg[from_all]
    new_pr   <- rep((1 - damp) / n_v, n_v)
    additions <- tapply(contrib, to_all, sum)
    new_pr[as.integer(names(additions))] <-
      new_pr[as.integer(names(additions))] + as.numeric(additions)
    new_pr / sum(new_pr)
  }

  pr <- Reduce(one_iter, seq_len(n_iter), init = rep(1 / n_v, n_v))
  stats::setNames(pr, vocab)
}

## Cross-attention proxy 4 — cosine similarity of phrase profile vs corpus centroid
cross_by_cosine <- function(raw_edges, vocab) {
  n_v <- length(vocab)
  if (is.null(raw_edges))
    return(stats::setNames(rep(0, n_v), vocab))
  v_idx    <- stats::setNames(seq_len(n_v), vocab)
  from_i   <- v_idx[raw_edges$from]
  to_i     <- v_idx[raw_edges$to]
  w        <- as.numeric(raw_edges$count)

  ## Build symmetric n_v × n_v co-occurrence matrix
  from_all <- c(from_i, to_i)
  to_all   <- c(to_i,   from_i)
  w_all    <- c(w, w)
  key_flat <- (from_all - 1L) * n_v + to_all
  agg_w    <- tapply(w_all, key_flat, sum)
  co_vec   <- numeric(n_v * n_v)
  co_vec[as.integer(names(agg_w))] <- as.numeric(agg_w)
  co_mat   <- matrix(co_vec, n_v, n_v)

  centroid  <- colMeans(co_mat)
  cent_norm <- sqrt(sum(centroid^2))
  if (cent_norm == 0) return(stats::setNames(rep(0, n_v), vocab))
  row_norms <- sqrt(rowSums(co_mat^2))
  row_norms[row_norms == 0] <- 1
  r_c <- drop(co_mat %*% centroid) / (row_norms * cent_norm)
  stats::setNames(r_c, vocab)
}

## Combine self-attention (YAKE) with a cross-attention proxy.
## s_c = ar_d * a_c + (1 - ar_d) * r_c  [both normalised to 0–1]
compute_attentionrank_scores <- function(doc_cands, vocab, n_docs, window,
                                         ar_d, ar_cross) {
  ## Self-attention: normalised YAKE
  yake  <- compute_phrase_scores(doc_cands, vocab, "yake", n_docs)
  max_y <- max(yake, na.rm = TRUE)
  a_c   <- if (max_y > 0) yake / max_y else yake

  ## Cross-attention proxy
  raw_edges <- raw_cooccurrence_edges(doc_cands, vocab, window)
  r_c <- switch(ar_cross,
    freq     = cross_by_freq(doc_cands, vocab, n_docs),
    degree   = cross_by_degree(raw_edges, vocab),
    pagerank = cross_by_pagerank(raw_edges, vocab),
    cosine   = cross_by_cosine(raw_edges, vocab),
    stop("ar_cross must be one of: freq, degree, pagerank, cosine", call. = FALSE)
  )
  max_r <- max(r_c, na.rm = TRUE)
  r_c   <- if (max_r > 0) r_c / max_r else r_c

  stats::setNames(ar_d * a_c + (1 - ar_d) * r_c, vocab)
}


## ── UDPipe helpers (pos = TRUE path) ─────────────────────────────────────────

## Universal POS tags allowed at phrase boundaries (start / end token)
.udpipe_boundary <- c("NOUN", "PROPN", "ADJ", "NUM")
## Tags allowed inside a multi-token phrase (e.g. "analysis of learning")
.udpipe_interior <- c("NOUN", "PROPN", "ADJ", "NUM", "ADP", "PART")

## Download or locate a cached UDPipe English model.
## Model is stored once in the user data dir so it survives R sessions.
udpipe_get_model <- function() {
  if (!requireNamespace("udpipe", quietly = TRUE))
    stop("Install the 'udpipe' package to use pos = TRUE:\n",
         "  install.packages('udpipe')", call. = FALSE)
  cache_dir <- tools::R_user_dir("bibnets", "data")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  found <- list.files(cache_dir, pattern = "\\.udpipe$", full.names = TRUE)
  if (length(found) > 0L) return(found[1L])
  message("Downloading UDPipe English model (~15 MB) to:\n  ", cache_dir)
  m <- udpipe::udpipe_download_model("english-ewt", model_dir = cache_dir)
  m$file_model
}

## Greedy longest-match extractor using UDPipe POS tags.
## Phrases must start and end on NOUN/PROPN/ADJ/NUM; ADP/PART allowed inside.
## Identical output schema to extract_abstract_candidates().
extract_candidates_pos <- function(ann_doc, n_range) {
  ann_doc <- ann_doc[!is.na(ann_doc$upos) &
                       !ann_doc$upos %in% c("PUNCT", "SPACE", "SYM"), ,
                     drop = FALSE]
  if (nrow(ann_doc) == 0L) return(.empty_cands())

  tokens <- tolower(ann_doc$token)
  orig_t <- ann_doc$token
  upos   <- ann_doc$upos
  n_tok  <- length(tokens)

  is_valid <- function(s, ng) {
    if (s + ng - 1L > n_tok) return(FALSE)
    tags <- upos[s:(s + ng - 1L)]
    if (!tags[1L] %in% .udpipe_boundary) return(FALSE)
    if (!tags[ng]  %in% .udpipe_boundary) return(FALSE)
    if (ng > 2L && !all(tags[2L:(ng - 1L)] %in% .udpipe_interior)) return(FALSE)
    TRUE
  }

  phrases   <- character(0L)
  positions <- integer(0L)
  ngrams    <- integer(0L)
  cap_rats  <- numeric(0L)

  s <- 1L
  while (s <= n_tok) {
    found <- FALSE
    for (ng in rev(n_range)) {
      if (ng %in% n_range && is_valid(s, ng)) {
        phrase    <- paste(tokens[s:(s + ng - 1L)], collapse = " ")
        cap_ratio <- mean(grepl("^[A-Z]", orig_t[s:(s + ng - 1L)]))
        phrases   <- c(phrases,   phrase)
        positions <- c(positions, s)
        ngrams    <- c(ngrams,    ng)
        cap_rats  <- c(cap_rats,  cap_ratio)
        s <- s + ng
        found <- TRUE
        break
      }
    }
    if (!found) s <- s + 1L
  }

  if (length(phrases) == 0L) return(.empty_cands())

  data.frame(
    phrase       = phrases,
    position     = positions,
    n_gram       = ngrams,
    cap_ratio    = cap_rats,
    total_tokens = n_tok,
    stringsAsFactors = FALSE
  )
}


## ── Candidate extractor for one abstract ─────────────────────────────────────

## Greedy longest-match scan: at each token position try the longest valid
## n-gram first; if found, emit it and advance by its length.
## This prevents "massive open", "open online", "massive open online" being
## extracted from the same span of text.
##
## Returns data.frame: phrase, position, n_gram, cap_ratio, total_tokens
extract_abstract_candidates <- function(text, n_range, hard_stops) {
  if (is.na(text) || nchar(trimws(text)) == 0L) return(.empty_cands())

  orig_tokens <- strsplit(
    gsub("[^a-zA-Z0-9\\s]", " ", text), "\\s+", fixed = FALSE
  )[[1L]]
  orig_tokens  <- orig_tokens[nchar(trimws(orig_tokens)) > 0L]
  lower_tokens <- tolower(orig_tokens)
  n_tok        <- length(lower_tokens)
  if (n_tok == 0L) return(.empty_cands())

  all_stops <- unique(c(hard_stops, .abstract_soft_stops))
  max_n     <- max(n_range)

  ## Check if an n-gram starting at position s of length ng is valid
  is_valid <- function(s, ng) {
    if (s + ng - 1L > n_tok) return(FALSE)
    toks <- lower_tokens[s:(s + ng - 1L)]
    first <- toks[1L]; last <- toks[ng]
    if (first %in% all_stops || nchar(first) <= 1L) return(FALSE)
    if (last  %in% all_stops || nchar(last)  <= 1L) return(FALSE)
    if (ng > 2L && any(toks[2L:(ng - 1L)] %in% hard_stops)) return(FALSE)
    TRUE
  }

  phrases   <- character(0L)
  positions <- integer(0L)
  ngrams    <- integer(0L)
  cap_rats  <- numeric(0L)

  s <- 1L
  while (s <= n_tok) {
    found <- FALSE
    ## Try longest n-gram first
    for (ng in rev(n_range)) {
      if (ng %in% n_range && is_valid(s, ng)) {
        phrase    <- paste(lower_tokens[s:(s + ng - 1L)], collapse = " ")
        cap_ratio <- mean(grepl("^[A-Z]", orig_tokens[s:(s + ng - 1L)]))
        phrases   <- c(phrases,   phrase)
        positions <- c(positions, s)
        ngrams    <- c(ngrams,    ng)
        cap_rats  <- c(cap_rats,  cap_ratio)
        s <- s + ng   ## advance past this phrase
        found <- TRUE
        break
      }
    }
    if (!found) s <- s + 1L
  }

  if (length(phrases) == 0L) return(.empty_cands())

  data.frame(
    phrase       = phrases,
    position     = positions,
    n_gram       = ngrams,
    cap_ratio    = cap_rats,
    total_tokens = n_tok,
    stringsAsFactors = FALSE
  )
}

.empty_cands <- function() {
  data.frame(phrase = character(0L), position = integer(0L),
             n_gram = integer(0L), cap_ratio = numeric(0L),
             total_tokens = integer(0L), stringsAsFactors = FALSE)
}


## ── Phrase quality scoring ────────────────────────────────────────────────────

compute_phrase_scores <- function(doc_cands, vocab, scoring, n_docs) {
  if (scoring == "none")
    return(stats::setNames(rep(1.0, length(vocab)), vocab))

  all_phrases <- unlist(lapply(doc_cands, `[[`, "phrase"), use.names = FALSE)
  all_pos     <- unlist(lapply(doc_cands, function(dc)
    dc$position / pmax(dc$total_tokens, 1L)), use.names = FALSE)
  all_cap     <- unlist(lapply(doc_cands, `[[`, "cap_ratio"), use.names = FALSE)
  all_docs    <- rep(seq_along(doc_cands),
                     vapply(doc_cands, nrow, integer(1L)))

  in_vocab    <- all_phrases %in% vocab
  all_phrases <- all_phrases[in_vocab]
  all_pos     <- all_pos[in_vocab]
  all_cap     <- all_cap[in_vocab]
  all_docs    <- all_docs[in_vocab]

  phrase_factor <- factor(all_phrases, levels = vocab)

  df <- vapply(vocab, function(p)
    length(unique(all_docs[all_phrases == p])), integer(1L))

  if (scoring == "freq")
    return(stats::setNames(as.numeric(df), vocab))

  idf <- log((n_docs + 1L) / (df + 1L))

  if (scoring == "idf")
    return(stats::setNames(pmax(idf, 0), vocab))

  ## YAKE-lite: IDF x position score x case score
  mean_pos   <- tapply(all_pos, phrase_factor, mean)
  pos_score  <- 1.0 / (1.0 + mean_pos)
  mean_cap   <- tapply(all_cap, phrase_factor, mean)
  case_score <- 1.0 + mean_cap

  stats::setNames(pmax(idf * pos_score * case_score, 0), vocab)
}


## ── Windowed co-occurrence for one abstract ───────────────────────────────────

## For each phrase occurrence, pair with every other phrase occurrence
## whose start token is within `window` tokens. Weight by distance decay.
compute_windowed_pairs <- function(dc, phrase_scores, window, decay) {
  dc <- dc[order(dc$position), , drop = FALSE]
  np <- nrow(dc)
  if (np < 2L) return(NULL)

  phrases <- dc$phrase
  pos     <- dc$position
  sq      <- phrase_scores[phrases]

  dist_mat <- abs(outer(pos, pos, `-`))
  pairs    <- which(dist_mat <= window & lower.tri(dist_mat), arr.ind = TRUE)
  if (nrow(pairs) == 0L) return(NULL)

  ii <- pairs[, 1L]
  jj <- pairs[, 2L]
  d  <- dist_mat[pairs]

  w    <- sq[ii] * sq[jj] * exp(-decay * d)
  keep <- is.finite(w) & w > 0
  if (!any(keep)) return(NULL)

  from_k <- phrases[ii[keep]]
  to_k   <- phrases[jj[keep]]
  w_k    <- w[keep]

  ## One contribution per phrase pair per abstract: keep closest co-occurrence
  sep      <- "\x01"
  pair_key <- paste(pmin(from_k, to_k), pmax(from_k, to_k), sep = sep)
  best_w   <- tapply(w_k, pair_key, max)

  ukeys <- names(best_w)
  parts <- strsplit(ukeys, sep, fixed = TRUE)
  data.frame(
    from   = vapply(parts, `[[`, character(1L), 1L),
    to     = vapply(parts, `[[`, character(1L), 2L),
    weight = as.numeric(best_w),
    stringsAsFactors = FALSE
  )
}


## ── Vocab consolidation ───────────────────────────────────────────────────────

## Remove any phrase that appears as a contiguous word sub-sequence of a
## longer phrase in the same vocab. Keeps only maximal phrases so that
## "massive open", "open online", "massive open online" are all dropped
## when "massive open online courses" is present.
consolidate_vocab <- function(vocab) {
  if (length(vocab) < 2L) return(vocab)

  ## Sort longest-first so we check against longer candidates first
  vocab <- vocab[order(-nchar(vocab))]

  ## Pre-build regex patterns for each phrase (word-boundary match)
  patterns <- vapply(vocab, function(p)
    paste0("(^| )", gsub("([.+*?\\^${}()|\\[\\]\\\\])", "\\\\\\1", p), "( |$)"),
    character(1L))

  is_sub <- vapply(seq_along(vocab), function(i) {
    p   <- vocab[i]
    pat <- patterns[i]
    ## Is p a sub-sequence of any LONGER phrase already in vocab?
    any(vapply(seq_len(i - 1L), function(j)
      nchar(vocab[j]) > nchar(p) && grepl(pat, vocab[j]),
      logical(1L)))
  }, logical(1L))

  vocab[!is_sub]
}


## ── Edge-list similarity normalisation ───────────────────────────────────────

normalize_edge_similarity <- function(edges, method) {
  str_from <- tapply(edges$weight, edges$from, sum)
  str_to   <- tapply(edges$weight, edges$to,   sum)
  wi  <- str_from[edges$from]; wi[is.na(wi)] <- 0
  wj  <- str_to[edges$to];     wj[is.na(wj)] <- 0
  cij <- edges$weight

  edges$weight <- switch(method,
    association = cij / (wi * wj),
    cosine      = cij / sqrt(wi * wj),
    jaccard     = cij / (wi + wj - cij),
    inclusion   = cij / pmin(wi, wj),
    equivalence = cij^2 / (wi * wj)
  )
  edges[is.finite(edges$weight) & edges$weight > 0, , drop = FALSE]
}


## ── Main function ─────────────────────────────────────────────────────────────

#' Build a concept co-occurrence network from abstracts
#'
#' Extracts multi-word phrase candidates (n-grams) from document abstracts,
#' filters by corpus frequency, scores each phrase using a YAKE-inspired
#' quality measure (IDF x position x case), then links phrases that co-occur
#' within a sliding token window inside each abstract. Closer phrases get
#' higher weight via exponential distance decay.
#'
#' Phrases are extracted using a soft/hard stopword split so prepositions are
#' allowed inside multi-word phrases — \dQuote{analysis of learning outcomes}
#' is a valid trigram even though \dQuote{of} is a stopword.
#'
#' @param data A data frame with \code{id} and an abstract text column.
#' @param field Character. Name of the text column. Default \code{"abstract"}.
#' @param n Integer vector. N-gram sizes to extract. Default \code{c(1L, 2L, 3L)}.
#' @param min_freq Integer. Minimum number of documents a phrase must appear
#'   in to be retained. Default \code{3L}.
#' @param window Integer. Co-occurrence window in tokens. Phrase pairs whose
#'   start positions are within this many tokens are linked. Default \code{10L}.
#' @param decay Numeric. Exponential distance decay rate per token:
#'   \code{exp(-decay * d)}. At \code{decay=1}, a phrase 3 tokens away
#'   gets 5\% weight; at 5 tokens, essentially zero. Default \code{1.0}.
#' @param scoring Character. Phrase quality scoring:
#'   \describe{
#'     \item{\code{"yake"}}{IDF x position score x case score (default).}
#'     \item{\code{"idf"}}{IDF only.}
#'     \item{\code{"freq"}}{Raw document frequency.}
#'     \item{\code{"none"}}{Equal weights.}
#'   }
#' @param similarity Character. Edge similarity normalisation. Default \code{"none"}.
#' @param threshold Numeric. Minimum edge weight. Default \code{0}.
#' @param top_n Integer or NULL. Keep only the top n edges by weight.
#' @param stopwords Character vector or NULL. Additional hard stopwords.
#' @param self_loops Logical. Include self-loops. Default \code{FALSE}.
#' @param format Character. Output format. Default \code{"edgelist"}.
#'
#' @return A \code{bibnets_network} data frame with columns \code{from},
#'   \code{to}, \code{weight}, \code{count}.
#'
#' @references
#' Campos, R., Mangaravite, V., Pasquali, A., Jorge, A., Nunes, C., &
#' Jatowt, A. (2020). YAKE! Keyword extraction from single documents using
#' multiple local features. \emph{Information Sciences}, 509, 257--289.
#' \doi{10.1016/j.ins.2019.09.013}
#'
#' @export
#' @examples
#' data(biblio_data)
#' abstract_network(biblio_data, min_freq = 1)
abstract_network <- function(data,
                              field        = "abstract",
                              n            = c(1L, 2L, 3L),
                              min_freq     = 3L,
                              min_count    = 1L,
                              window       = 10L,
                              decay        = 1.0,
                              scoring      = "yake",
                              ar_d         = 0.8,
                              ar_cross     = "degree",
                              count_weight = 0,
                              similarity = "none",
                              threshold  = 0,
                              top_n      = NULL,
                              stopwords  = NULL,
                              self_loops = FALSE,
                              clean      = FALSE,
                              pos        = FALSE,
                              format     = "edgelist") {

  check_data(data, c("id", field))
  check_choice(scoring,
               c("yake", "idf", "freq", "none", "attentionrank"), "scoring")
  check_choice(ar_cross, c("freq", "degree", "pagerank", "cosine"), "ar_cross")
  check_choice(similarity, c("none", "association", "cosine", "jaccard",
                              "inclusion", "equivalence"), "similarity")
  check_format(format)

  texts      <- as.character(data[[field]])
  if (clean) texts <- vapply(texts, strip_abstract_noise, character(1L),
                             USE.NAMES = FALSE)
  n_docs     <- length(texts)
  n_range    <- sort(unique(as.integer(n)))
  hard_stops <- unique(c(.abstract_hard_stops, tolower(trimws(stopwords))))

  ## ── Pass 1: extract candidates ────────────────────────────────────────────

  if (pos) {
    model <- udpipe::udpipe_load_model(udpipe_get_model())
    message("Annotating ", n_docs, " abstracts with UDPipe...")
    ann <- as.data.frame(udpipe::udpipe_annotate(
      model, x = texts, doc_id = as.character(seq_along(texts))
    ))
    doc_cands <- lapply(seq_along(texts), function(i)
      extract_candidates_pos(
        ann[ann$doc_id == as.character(i), , drop = FALSE],
        n_range
      )
    )
  } else {
    doc_cands <- lapply(texts, extract_abstract_candidates,
                        n_range = n_range, hard_stops = hard_stops)
  }

  phrase_df_vec <- table(
    unlist(lapply(doc_cands, function(dc) unique(dc$phrase)), use.names = FALSE)
  )
  keep_phrases <- names(phrase_df_vec)[phrase_df_vec >= min_freq]

  ## Consolidation: drop sub-phrases that have a longer superphrase in vocab.
  ## "massive open" is removed if "massive open online courses" is in vocab.
  keep_phrases <- consolidate_vocab(keep_phrases)

  if (length(keep_phrases) < 2L)
    return(as_bibnets_network(.empty_abstract_edges(),
                              network_type = "abstract_cooccurrence",
                              counting = scoring, similarity = similarity,
                              format = format))

  ## ── Phrase scoring ────────────────────────────────────────────────────────

  phrase_scores <- if (scoring == "attentionrank") {
    compute_attentionrank_scores(
      doc_cands  = doc_cands, vocab = keep_phrases,
      n_docs     = n_docs,   window = window,
      ar_d       = ar_d,     ar_cross = ar_cross
    )
  } else {
    compute_phrase_scores(
      doc_cands = doc_cands, vocab = keep_phrases,
      scoring = scoring, n_docs = n_docs
    )
  }

  ## ── Pass 2: windowed co-occurrence per abstract ───────────────────────────

  edge_list <- lapply(doc_cands, function(dc) {
    dc <- dc[dc$phrase %in% keep_phrases, , drop = FALSE]
    if (nrow(dc) < 2L) return(NULL)
    compute_windowed_pairs(dc, phrase_scores, window, decay)
  })

  edge_list <- edge_list[!vapply(edge_list, is.null, logical(1L))]

  if (length(edge_list) == 0L)
    return(as_bibnets_network(.empty_abstract_edges(),
                              network_type = "abstract_cooccurrence",
                              counting = scoring, similarity = similarity,
                              format = format))

  ## ── Aggregate ─────────────────────────────────────────────────────────────

  all_edges <- do.call(rbind, edge_list)
  sep       <- "|||"
  pair_key  <- paste(pmin(all_edges$from, all_edges$to),
                     pmax(all_edges$from, all_edges$to), sep = sep)

  weight_agg <- vapply(split(all_edges$weight, pair_key), sum,    numeric(1L))
  count_agg  <- vapply(split(all_edges$weight, pair_key), length, integer(1L))

  parts  <- strsplit(names(weight_agg), sep, fixed = TRUE)
  result <- data.frame(
    from   = vapply(parts, `[[`, character(1L), 1L),
    to     = vapply(parts, `[[`, character(1L), 2L),
    weight = unname(weight_agg),
    count  = unname(count_agg),
    stringsAsFactors = FALSE
  )

  if (!self_loops)
    result <- result[result$from != result$to, , drop = FALSE]

  result <- result[result$count >= as.integer(min_count), , drop = FALSE]

  if (count_weight != 0)
    result$weight <- result$weight * result$count ^ count_weight

  if (similarity != "none")
    result <- normalize_edge_similarity(result, similarity)

  result <- result[is.finite(result$weight) & result$weight > threshold, ,
                   drop = FALSE]
  result <- result[order(-result$weight), , drop = FALSE]

  if (!is.null(top_n))
    result <- head(result, as.integer(top_n))

  rownames(result) <- NULL

  as_bibnets_network(result,
                     network_type = "abstract_cooccurrence",
                     counting = scoring, similarity = similarity,
                     format = format)
}

.empty_abstract_edges <- function() {
  data.frame(from = character(0L), to = character(0L),
             weight = numeric(0L), count = integer(0L),
             stringsAsFactors = FALSE)
}
