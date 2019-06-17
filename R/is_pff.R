#' Is the filename a Peregrine-friendly filename?
#' @param filename name of the file
#' @export
is_pff <- function(filename) {
  all(is.na(stringr::str_match(string = filename, pattern = "(/local)?/tmp/")))
}
