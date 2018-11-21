#' @title Creates file containing marginal loglikelihoods
#' @description Creates file containing marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return the name of the saved file
#' @author Giovanni Laudanno
#' @export
raz_create_marg_liks_file <- function(
  project_folder_name
) {

  # create the dataframe
  marg_liks <- raz_collect_marg_liks(project_folder_name) # nolint internal function

  # save output
  results_folder <- file.path(
    project_folder_name,
    "results"
  )
  if (!dir.exists(results_folder)) {
    dir.create(results_folder)
  }
  marg_liks_filename <- file.path(
    results_folder,
    "marg_liks.csv"
  )
  utils::write.csv(x = marg_liks, file = marg_liks_filename)

  # return file name
  marg_liks_filename
}
