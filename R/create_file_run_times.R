#' Creates file containing the run times
#' @inheritParams default_params_doc
#' @return the name of the saved file
#' @author Richel J.C. Bilderbeek
#' @aliases create_file_run_times create_run_times_file
#' @export create_file_run_times create_run_times_file
create_file_run_times <- create_run_times_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  run_times <- collect_run_times(project_folder_name) # nolint internal function

  # save output
  results_folder <- get_results_path(project_folder_name) # nolint
  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)

  run_times_filename <- file.path(
    results_folder,
    "run_times.csv"
  )
  utils::write.csv(x = run_times, file = run_times_filename)

  # return file name
  run_times_filename
}
