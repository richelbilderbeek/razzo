#' Is the filename a Peregrine-friendly filename?
#' @export
is_pff <- function(filename) {
  all(is.na(stringr::str_match(string = filename, pattern = "(/local)?/tmp/")))
}
