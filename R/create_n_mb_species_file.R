#' Create the dataframe with number of multiple-born species 
#' and save it as a file
#' @inheritParams default_params_doc
#' @return the filename
#' @author Richel J.C. Bilderbeek
#' @export
create_n_mb_species_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  df_n_mb_species <- collect_n_mb_species(project_folder_name) # nolint internal function

  # save output
  results_folder <- get_results_path(project_folder_name) # nolint

  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)

  n_mb_species_filename <- file.path(
    results_folder,
    "n_mb_species.csv"
  )
  utils::write.csv(x = df_n_mb_species, file = n_mb_species_filename)

  # return file name
  n_mb_species_filename
}
