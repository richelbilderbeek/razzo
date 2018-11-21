#' Create a file that stores the results of a marginal likelihood estimation
#' of an BD alignment
#' @inheritParams default_params_doc
#' @return name of the created file
#' @author Richel J.C. Bilderbeek
#' @export
create_bd_marg_lik_file <- function(
  parameters_filename
) {
  # Check environment
  testit::assert(beastier::is_beast2_installed())
  testit::assert(mauricer::mrc_is_installed("NS"))
  testit::assert(rappdirs::app_dir()$os != "win")

  # Check input
  parameters <- open_parameters_file(parameters_filename) # nolint internal function
  bd_alignment_filename <- file.path(dirname(parameters_filename), "bd.fasta")
  testit::assert(file.exists(bd_alignment_filename))

  # Create
  marg_lik <- est_marg_lik( # nolint internal function
    parameters = parameters,
    alignment = ape::read.FASTA(bd_alignment_filename)
  )
  bd_marg_lik_filename <- file.path(
    dirname(parameters_filename),
    "bd_marg_lik.csv"
  )

  # Save
  utils::write.csv(x = marg_lik, file = bd_marg_lik_filename)

  bd_marg_lik_filename
}
