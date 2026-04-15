## Create example bibliometric dataset for citenets package
## A small but realistic dataset of 10 papers with overlapping
## authors, references, and keywords — enough to test all network types.

set.seed(42)

biblio_data <- data.frame(
  id = paste0("W", 1:10),
  title = c(
    "Co-citation analysis of scientific literature",
    "Bibliographic coupling and research fronts",
    "Fractional counting in bibliometrics",
    "Network analysis of scholarly communication",
    "Community detection in citation networks",
    "Author name disambiguation methods",
    "Keyword co-occurrence mapping",
    "Temporal patterns in citation data",
    "Normalization of co-occurrence data",
    "Mapping scientific knowledge domains"
  ),
  year = c(2018L, 2019L, 2020L, 2019L, 2021L,
           2020L, 2018L, 2022L, 2019L, 2021L),
  journal = c(
    "Scientometrics", "Journal of Informetrics", "Scientometrics",
    "JASIST", "Journal of Informetrics", "Scientometrics",
    "JASIST", "Quantitative Science Studies", "JASIST",
    "Journal of Informetrics"
  ),
  doi = paste0("10.1000/test.", 1:10),
  cited_by_count = c(45L, 32L, 18L, 55L, 12L, 8L, 22L, 5L, 38L, 15L),
  stringsAsFactors = FALSE
)

biblio_data$authors <- list(
  c("Smith J", "Jones A", "Lee K"),
  c("Smith J", "Brown M"),
  c("Jones A", "Lee K", "Chen W"),
  c("Brown M", "Davis R", "Smith J"),
  c("Lee K", "Chen W"),
  c("Davis R", "Jones A"),
  c("Smith J", "Brown M", "Lee K"),
  c("Chen W", "Davis R"),
  c("Jones A", "Smith J"),
  c("Brown M", "Chen W", "Lee K")
)

biblio_data$references <- list(
  c("R1", "R2", "R3", "R5"),
  c("R1", "R2", "R4", "R6"),
  c("R2", "R3", "R5", "R7"),
  c("R1", "R3", "R4", "R8"),
  c("R2", "R5", "R7", "R9"),
  c("R3", "R4", "R6", "R8"),
  c("R1", "R2", "R5", "R10"),
  c("R5", "R7", "R9", "R10"),
  c("R1", "R2", "R3", "R4"),
  c("R2", "R3", "R6", "R10")
)

biblio_data$keywords <- list(
  c("co-citation", "bibliometrics", "network analysis"),
  c("coupling", "research fronts", "bibliometrics"),
  c("fractional counting", "normalization", "bibliometrics"),
  c("network analysis", "scholarly communication", "co-authorship"),
  c("community detection", "citation networks", "clustering"),
  c("disambiguation", "author names", "entity resolution"),
  c("keyword mapping", "co-occurrence", "bibliometrics"),
  c("temporal analysis", "citation patterns", "dynamics"),
  c("normalization", "similarity measures", "co-occurrence"),
  c("science mapping", "knowledge domains", "bibliometrics")
)

usethis::use_data(biblio_data, overwrite = TRUE)
