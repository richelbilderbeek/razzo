#' Create the parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_misc_params <- function(
  tree_filename = get_pff_tempfile(fileext = ".tree")
) {
  testit::assert(razzo::is_pff(tree_filename))
  misc_params <- list(
    tree_filename = tree_filename
  )
  check_misc_params(misc_params) # nolint razzo function
  misc_params
}
