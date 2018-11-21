#' Copies a file from \code{inst/extdata} and puts it in
#' a temporary folder.
#' Will stop if the file is absent in \code{inst/extdata}.
#' @param filename name of the file in \code{inst/extdata}
#' @return the name of the temporary file. Will also copy the
#'   \code{inst/extdata} file to that temporary folder.
#' @author Richel J.C. Bilderbeek
#' @examples
#'   filename <- create_tempfile("parameters.csv")
#'   testthat::expect_true(file.exists(filename))
#' @export
create_tempfile <- function(filename) {
  to <- file.path(tempdir(), filename)
  file.copy(
    from = get_path(filename),
    to = to
  )
  to
}
