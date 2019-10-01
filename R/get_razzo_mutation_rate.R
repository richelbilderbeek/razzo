#' Get the razzo mutation rate, 
#' as used in the article
#' @export
get_razzo_mutation_rate <- function() {
  0.5 / get_razzo_crown_age()
}
