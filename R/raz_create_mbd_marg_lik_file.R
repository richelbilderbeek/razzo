#' Create a file that stores the results of a marginal likelihood estimation
#' of an MBD alignment
#' @inheritParams default_params_doc
#' @return name of the created file
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_mbd_marg_lik_file <- function(
  parameters_filename
) {
  # Check environment
  testit::assert(beastier::is_beast2_installed())
  testit::assert(mauricer::mrc_is_installed("NS"))
  testit::assert(rappdirs::app_dir()$os != "win")

  # Check input
  parameters <- raz_open_parameters_file(parameters_filename) # nolint internal function
  mbd_alignment_filename <- file.path(dirname(parameters_filename), "mbd.fasta")
  testit::assert(file.exists(mbd_alignment_filename))

  # Create
  marg_lik <- raz_est_marg_lik( # nolint internal function
    parameters = parameters,
    alignment = ape::read.FASTA(mbd_alignment_filename)
  )
  mbd_marg_lik_filename <- file.path(
    dirname(parameters_filename),
    "mbd_marg_lik.csv"
  )

  # Save
  utils::write.csv(x = marg_lik, file = mbd_marg_lik_filename)

  mbd_marg_lik_filename
}
