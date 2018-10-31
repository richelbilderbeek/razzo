#' Create the inference files from a FASTA file.
#' For example, for a FASTA file named '/my_folder/mbd.fasta', this
#' function will create:
#' \itemize{
#'   \item '/my_folder/bd.trees' BEAST2 posterior trees
#'   \item '/my_folder/bd.log' BEAST2 parameter estimates
#'   \item '/my_folder/bd_mar_lik.csv' BEAST2 marginal likelihood estimate
#' }
#' Assumes, for a FASTA file named '/my_folder/bd.fasta', this
#'   that there is a parameters file named '/my_folder/parameters.csv'
#' @inheritParams default_params_doc
#' @return names of the files created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_bd_posterior_files <- function(
  parameters_filename
) {
  raz_check_file_exists(parameters_filename)
  bd_alignment_filename <- file.path(dirname(parameters_filename), "bd.fasta")
  raz_check_file_exists(bd_alignment_filename)

  bd_posterior <- raz_create_posterior(
    parameters = raz_open_parameters_file(parameters_filename),
    alignment = ape::read.FASTA(bd_alignment_filename)
  )

  bd_trees_filename <- file.path(dirname(parameters_filename), "bd.trees")
  bd_log_filename <- file.path(dirname(parameters_filename), "bd.log")
  bd_mar_log_lik_filename <- file.path(
    dirname(parameters_filename), "bd_mar_log_lik.csv")

  ape::write.tree(phy = bd_posterior$trees, file = bd_trees_filename)
  utils::write.csv(x = bd_posterior$estimates, file = bd_log_filename)
  utils::write.csv(x = bd_posterior$ns, file = bd_mar_log_lik_filename)

  # Return the filenames
  c(bd_trees_filename, bd_log_filename, bd_mar_log_lik_filename)
}
