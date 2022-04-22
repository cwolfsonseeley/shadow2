config <- function(query) {
    shadowdir <- "R:/Prospect Development/Prospect Analysis/external_datasets/"
    acs <- paste0(shadowdir, "acs_demographics.csv")

    get(query, inherits = FALSE)
}
