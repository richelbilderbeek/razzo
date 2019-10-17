#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_parameters_files <- function(
  project_folder_name = getwd(),
  experiment_type
) {
  testit::assert(peregrine::is_pff(project_folder_name))
  testit::assert(experiment_type == "test" || experiment_type == "full")

  # Pick the right set of razzo_paramses
  razzo_paramses <- NA
  if (experiment_type == "test") {
    razzo_paramses <- create_test_razzo_paramses(
      project_folder_name = project_folder_name
    )
  } else {
    testit::assert(experiment_type == "full")
    razzo_paramses <- create_razzo_paramses(
      project_folder_name = project_folder_name
    )
  }
  testit::assert(!beautier::is_one_na(razzo_paramses))

  # Save it to file, returns filenames
  save_razzo_paramses(razzo_paramses)
}
