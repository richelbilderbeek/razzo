#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_parameters_files <- function(
  project_folder_name = getwd(),
  experiment_type = "test"
) {
  testit::assert(peregrine::is_pff(project_folder_name))
  testit::assert(experiment_type == "test" || experiment_type == "full")
  n_replicates <- 2
  if (experiment_type == "full") {
    # For now
    n_replicates <- 4
  }
  save_razzo_paramses(
    create_razzo_paramses(
      project_folder_name = project_folder_name,
      mbd_paramses = create_mbd_paramses(
        n_replicates = n_replicates
      )
    )
  )
}

#' Save the list of \code{razzo_params} to the exected locations
#'
#' A \code{razzo_params} already hold its desired location, which
#' if the folder where
#' \code{razzo_params$pir_params$alignment_params$fasta_filename}
#' will be stored. That foldername already follows the
#' \code{project_folder_name/data/[settings]/seed/[models]}
#' file storage location convention.
#' This function will create a \code{parameters.RDa}
#' in that folder.
#' @inheritParams default_params_doc
#' @return the paths to each created \code{parameters.RDa} file
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
save_razzo_paramses <- function(
  razzo_paramses = create_razzo_paramses(
    project_folder_name = peregrine::get_pff_tempfile()
  )
) {
  parameters_filenames <- rep(NA, length(razzo_paramses))
  for (i in seq_along(razzo_paramses)) {
    razzo_params <- razzo_paramses[[i]]
    # The parameters.RDa file must be saved in the same folder as the
    # FASTA filename (of the true alignment)
    fasta_filename <- razzo_params$pir_params$alignment_params$fasta_filename

    # Create the folder to store the parameters.RDa file in,
    # do not warn if it already exists
    folder_name <- dirname(fasta_filename)
    dir.create(folder_name, showWarnings = FALSE, recursive = TRUE)
    parameters_filename <- file.path(folder_name, "parameters.RDa")
    saveRDS(object = razzo_params, file = parameters_filename)
    testthat::expect_silent(
      check_razzo_params(readRDS(parameters_filename))
    )
    parameters_filenames[i] <- parameters_filename
  }
  parameters_filenames
}
