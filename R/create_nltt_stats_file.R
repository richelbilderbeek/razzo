#' @title Creates file containing nltt stats
#' @description Creates file containing nltt stats
#' @inheritParams default_params_doc
#' @return the name of the saved file
#' @author Giovanni Laudanno
#' @export
create_nltt_stats_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  nltt_stats <- collect_nltt_stats(project_folder_name) # nolint internal function

  # save output
  results_folder <- get_results_path(project_folder_name) # nolint

  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)

  nltt_stats_filename <- file.path(
    results_folder,
    "nltt_stats.csv"
  )
  utils::write.csv(x = nltt_stats, file = nltt_stats_filename)

  # return file name
  nltt_stats_filename
}
