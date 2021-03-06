#' Create the dataframe with the Effective Samples Sizes (ESSes)
#' and save it as a file
#' @inheritParams default_params_doc
#' @return the filename
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @aliases create_file_esses create_esses_file
#' @export create_file_esses create_esses_file
create_file_esses <- create_esses_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  df_esses <- razzo::collect_esses(project_folder_name)

  # save output
  results_folder <- razzo::get_results_path(project_folder_name)

  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)

  esses_filename <- file.path(
    results_folder,
    "esses.csv"
  )
  utils::write.csv(x = df_esses, file = esses_filename)

  # return file name
  esses_filename
}
