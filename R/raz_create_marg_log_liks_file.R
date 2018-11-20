#' @title Creates file containing marginal loglikelihoods
#' @description Creates file containing marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return the name of the saved file
#' @author Giovanni Laudanno
#' @export
raz_create_marg_log_liks_file <- function(
  project_folder_name
) {

  # create the dataframe
  marg_log_liks <- raz_collect_marg_log_liks(project_folder_name)

  # save output
  results_folder <- file.path(
    project_folder_name,
    "results_folder"
  )
  if (!dir.exists(results_folder)) {
    dir.create(results_folder)
  }
  marg_log_liks_filename <- file.path(
    results_folder,
    "marg_log_liks.csv"
  )
  utils::write.csv(x = marg_log_liks, file = marg_log_liks_filename)

  # return file name
  marg_log_liks_filename
}
