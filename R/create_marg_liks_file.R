#' @title Creates file containing marginal loglikelihoods
#' @description Creates file containing marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return the name of the saved file
#' @author Giovanni Laudanno
#' @export
create_marg_liks_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  marg_liks <- collect_marg_liks(project_folder_name) # nolint internal function

  # save output
  results_folder <- get_results_path(project_folder_name) # nolint
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
