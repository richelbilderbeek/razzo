#' Create the dataframe with esses data
#' and save it as a file
#' @inheritParams default_params_doc
#' @return the esses
#' @author Giovanni Laudanno
#' @export
create_esses_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  df_esses <- collect_esses(project_folder_name) # nolint internal function

  # save output
  results_folder <- get_results_path(project_folder_name) # nolint
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
