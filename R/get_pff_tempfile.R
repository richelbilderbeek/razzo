#' Get a Peregrine-friendly temporary filename
#' @param pattern see \link{tempfile}
#' @param pff_tmpdir Peregrine-friendly temporary directory name,
#'   see \link{tempfile}
#' @param fileext see \link{tempfile}
#' @export
#' @author Richel J.C. Bilderbeek
get_pff_tempfile <- function(
  pattern = "razzo_file_",
  pff_tmpdir = rappdirs::user_cache_dir(),
  fileext = ""
) {
  testit::assert(is_pff(pff_tmpdir))
  filename <- tempfile(
    pattern = pattern,
    tmpdir = pff_tmpdir,
    fileext = fileext
  )
  testit::assert(!file.exists(filename))
  testit::assert(is_pff(filename))
  filename
}
