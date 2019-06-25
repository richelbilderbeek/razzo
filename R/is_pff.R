#' Is the filename a Peregrine-friendly filename?
#' @param filename name of the file
#' @seealso see \link{get_pff_tempdir} and \link{get_pff_tempfile}
#' to get a Peregrine-friendly temporary folder and filename
#' @author Richel J.C. Bilderbeek
#' @export
is_pff <- function(filename) {
  all(is.na(stringr::str_match(string = filename, pattern = "(/local)?/tmp/")))
}
