#' Copies a file from \code{inst/extdata} and puts it in
#' a temporary folder.
#' Will stop if the file is absent in \code{inst/extdata}.
#' @param filename name of the file in \code{inst/extdata}
#' @return the name of the temporary file. Will also copy the
#'   \code{inst/extdata} file to that temporary folder.
#' @author Richel J.C. Bilderbeek
#' @examples
#'   filename <- raz_create_tempfile("parameters.csv")
#'   testthat::expect_true(file.exists(filename))
#' @export
raz_create_tempfile <- function(filename) {
  to <- file.path(raz_make_tempdir(), filename)
  file.copy(
    from = raz_get_path(filename),
    to = to
  )
  to
}
