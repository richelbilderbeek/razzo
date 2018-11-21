#' Create the dataframe with esses data
#' and save it as a file
#' @inheritParams default_params_doc
#' @return the esses
#' @author Giovanni Laudanno
#' @export
raz_create_esses_file <- function(
  project_folder_name
) {
  check_project_folder_name(project_folder_name) # nolint internal function

  # create the dataframe
  df_esses <- raz_collect_esses(project_folder_name) # nolint internal function

  # save output
  results_folder <- file.path(
    project_folder_name,
    "results"
  )
  if (!dir.exists(results_folder)) {
    dir.create(results_folder)
  }
  esses_filename <- file.path(
    results_folder,
    "esses.csv"
  )
  utils::write.csv(x = df_esses, file = esses_filename)

  # return file name
  esses_filename
}
