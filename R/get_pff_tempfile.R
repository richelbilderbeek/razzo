#' Get a Peregrine-friendly temporary filename
#' @param pattern see \link{tempfile}
#' @param pff_tmpdir Peregrine-friendly temporary directory name,
#'   see \link{get_pff_tempdir}
#' @param fileext see \link{tempfile}
#' @seealso use \link{get_pff_tempdir}
#' to get a Peregrine-friendly folder name
#' @author Richel J.C. Bilderbeek
#' @export
get_pff_tempfile <- function(
  pattern = "razzo_file_",
  pff_tmpdir = get_pff_tempdir(),
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
