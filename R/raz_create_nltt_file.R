#' Create the NLTT statistics distribution files from a posterior file.
#' Assumes for a posterior file named '1x.trees'
#'   that there is a 'true' tree file called '1x.tree'
#' @return name of the nLTT file created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_nltt_file <- function(trees_filename) {

  # TODO: trees_filename should exist one day :-)
  if (1 == 2) {
    testit::assert(file.exists(trees_filename))
  }

  base <- tools::file_path_sans_ext(trees_filename)
  nltt_filename <- paste0(c(base, "_nltts.csv"), collapse = "")

  # TODO: create the nLTT file
  if (1 == 2) {

  }

  nltt_filename
}
