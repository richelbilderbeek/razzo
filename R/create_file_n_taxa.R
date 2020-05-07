#' Creates file containing the number of taxa
#' @inheritParams default_params_doc
#' @return the name of the saved file
#' @author Richèl J.C. Bilderbeek
#' @aliases create_file_n_taxa create_n_taxa_file
#' @export create_file_n_taxa create_n_taxa_file
create_file_n_taxa <- create_n_taxa_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  n_taxa <- razzo::collect_n_taxa(project_folder_name)

  # save output
  results_folder <- razzo::get_results_path(project_folder_name)

  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)

  n_taxa_filename <- file.path(
    results_folder,
    "n_taxa.csv"
  )
  utils::write.csv(x = n_taxa, file = n_taxa_filename)

  # return file name
  n_taxa_filename
}
