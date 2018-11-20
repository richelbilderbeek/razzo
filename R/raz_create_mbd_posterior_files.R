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
raz_create_mbd_posterior_files <- function(
  parameters_filename
) {
  raz_check_file_exists(parameters_filename) # nolint internal function
  mbd_alignment_filename <- file.path(dirname(parameters_filename), "mbd.fasta")
  raz_check_file_exists(mbd_alignment_filename) # nolint internal function

  mbd_posterior <- raz_create_posterior(
    parameters = raz_open_parameters_file(parameters_filename),
    alignment = ape::read.FASTA(mbd_alignment_filename)
  )

  mbd_trees_filename <- file.path(dirname(parameters_filename), "mbd.trees")
  mbd_log_filename <- file.path(dirname(parameters_filename), "mbd.log")

  tracerer::save_beast_trees(
    trees = mbd_posterior$trees,
    filename = mbd_trees_filename
  )
  tracerer::save_beast_estimates(
    estimates = mbd_posterior$estimates,
    filename = mbd_log_filename
  )

  # Return the filenames
  c(mbd_trees_filename, mbd_log_filename)
}
