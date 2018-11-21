#' Get the full path of a file in the \code{inst/extdata} folder
#' @inheritParams default_params_doc
#' @return the full path of the filename, if and only if
#'   the file is present. Will stop otherwise.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'   testit::assert(is.character(get_path("parameters.csv")))
#' @export
get_path <- function(filename) {

  if (grepl("P274829", system.file(package = "razzo"))) {
    file.copy(
      from = file.path(
        "F:/Dropbox", # nolint
        "University",
        "Progress",
        "RQ3-razzo",
        "razzo",
        "inst",
        "extdata"
      ),
      to = system.file("extdata", package = "razzo"),
      overwrite = TRUE,
      recursive = FALSE,
      copy.mode = TRUE,
      copy.date = FALSE
    )
  }

  full <- system.file("extdata", filename, package = "razzo")
  if (!file.exists(full)) {
    stop("'filename' must be the name of a file in 'inst/extdata'")
  }
  full
}
