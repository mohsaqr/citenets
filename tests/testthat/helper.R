## Test helpers for citenets

## Small test data: 3 papers, 3 authors, 4 references
make_test_data <- function() {
  d <- data.frame(
    id = c("W1", "W2", "W3"),
    title = c("Paper A", "Paper B", "Paper C"),
    year = c(2020L, 2021L, 2022L),
    journal = c("J1", "J1", "J2"),
    stringsAsFactors = FALSE
  )
  d$authors <- list(
    c("Alice", "Bob"),       # W1: 2 authors
    c("Alice", "Carol"),     # W2: 2 authors
    c("Bob", "Carol", "Dan") # W3: 3 authors
  )
  d$references <- list(
    c("R1", "R2", "R3"),     # W1: 3 refs
    c("R1", "R2", "R4"),     # W2: 3 refs
    c("R2", "R3", "R4")      # W3: 3 refs
  )
  d$keywords <- list(
    c("ml", "dl"),
    c("ml", "nlp"),
    c("dl", "nlp", "cv")
  )
  d
}

## Tolerance for floating point comparisons
tol <- 1e-10
