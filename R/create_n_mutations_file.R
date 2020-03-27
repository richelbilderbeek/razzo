#' Creates file containing the number of mutations
#' @inheritParams default_params_doc
#' @return the name of the saved file
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @aliases create_file_n_mutations create_n_mutations_file
#' @export create_file_n_mutations create_n_mutations_file
create_file_n_mutations <- create_n_mutations_file <- function(
  project_folder_name = getwd()
) {

  # create the dataframe
  n_mutations <- razzo::collect_n_mutations(project_folder_name)

  # save output
  results_folder <- razzo::get_results_path(project_folder_name)

  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)

  n_mutations_filename <- file.path(
    results_folder,
    "n_mutations.csv"
  )
  utils::write.csv(x = n_mutations, file = n_mutations_filename)

  # return file name
  n_mutations_filename
}
