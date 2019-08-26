#' Create the parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_misc_params <- function(
  folder_name = peregrine::get_pff_tempfile()
) {
  testit::assert(peregrine::is_pff(folder_name))
  misc_params <- list(
    tree_filename = file.path(folder_name, "mbd.tree")
  )
  check_misc_params(misc_params) # nolint razzo function
  misc_params
}
