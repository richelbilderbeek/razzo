#' Function to check if a file exists.
#' Calls \code{stop} if the file is absent
#' @param filename name of the file
#' @return nothing. Will \code{stop} if the file is absent,
#'   with a proper error message
#' @author Richel J.C. Bilderbeek
#' @noRd
check_file_exists <- function(filename) {
  if (!file.exists(filename)) {
    stop(
      "File not found. Could not find file with path '",
      filename,
      "'"
    )
  }
}
