#' Get a Peregrine-friendly temporary directory
#' @export
#' @author Richel J.C. Bilderbeek
get_pff_tempdir <- function() {
  dirname <- file.path(
    rappdirs::user_cache_dir(),
    basename(tempfile())
  )
  testit::assert(is_pff(dirname))
  dirname
}
