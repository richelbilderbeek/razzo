#' Create the parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @param razzo_version version of razzo this parameter file is created with
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_misc_params <- function(
  folder_name = peregrine::get_pff_tempfile(),
  razzo_version = as.character(utils::packageVersion("razzo"))
) {
  testit::assert(peregrine::is_pff(folder_name))
  misc_params <- list(
    tree_filename = razzo::get_tree_filename(
      folder_name = folder_name,
      tree_type = "true"
    ),
    razzo_version = razzo_version
  )
  razzo::check_misc_params(misc_params)
  misc_params
}
