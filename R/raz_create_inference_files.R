#' Create the inference files from a FASTA file.
#' For example, for a FASTA file named '/my_folder/mbd.fasta', this
#' function will create:
#' \itemize{
#'   \item '/my_folder/mbd.trees' BEAST2 posterior trees
#'   \item '/my_folder/mbd.log' BEAST2 parameter estimates
#'   \item '/my_folder/mbd_mar_lik.csv' BEAST2 marginal likelihood estimate
#' }
#' Assumes, for a FASTA file named '/my_folder/mbd.fasta', this
#'   that there is a parameters file named '/my_folder/parameters.csv'
#' @inheritParams default_params_doc
#' @return names of the files created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_inference_files <- function(fasta_filename)
{
  # TODO: fasta_filename should exist one day :-)
  if (1 == 2) {
    testit::assert(file.exists(fasta_filename))
  }

  parameters_filename <- file.path(dirname(fasta_filename), "parameters.csv")

  # TODO: parameter file should exist one day :-)
  if (1 == 2) {
    testit::assert(file.exists(parameters_filename))
  }

  # Read the parameters
  parameters <- raz_open_parameters_file(parameters_filename)
  testit::assert(parameters$lambda >= 0.0)

  base <- tools::file_path_sans_ext(fasta_filename)
  trees_filename <- paste0(c(base, ".trees"), collapse = "")
  log_filename <- paste0(c(base, ".log"), collapse = "")
  mar_lik_filename <- paste0(c(base, "_mar_lik.csv"), collapse = "")

  # TODO: create the BEAST2 posterior trees, parameter estimates
  # and marginal likelihood files


  inference_filenames <- c(
    trees_filename,
    log_filename,
    mar_lik_filename
  )
  inference_filenames
}
