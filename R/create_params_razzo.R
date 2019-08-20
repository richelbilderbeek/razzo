#' Create the parameters for one experiment.
#'
#' One \code{razzo_params} runs one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @aliases create_params_razzo create_razzo_params
#' @export create_params_razzo create_razzo_params
create_params_razzo <- create_razzo_params <- function(
  mbd_params,
  pir_params,
  misc_params
) {
  razzo_params <- list(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )
  check_razzo_params(razzo_params) # nolint razzo function
  razzo_params
}

#' Create one set of parameter for one \code{razzo} experiment.
#'
#' All the files follow the expected naming scheme,
#' using the \link{dirname} of
#' \code{pir_params$alignment_params$fasta_filename} as
#' the folder where all files will be created
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_test_razzo_params <- function(
  mbd_params = create_test_mbd_params(),
  pir_params = peregrine::create_test_pff_pir_params(
    twinning_params = peregrine::create_pff_twinning_params()
  ),
  misc_params = create_misc_params()
) {
  check_mbd_params(mbd_params)
  pirouette::check_pir_params(pir_params)
  check_misc_params(misc_params)
  folder_name <- dirname(pir_params$alignment_params$fasta_filename)
  misc_params$tree_filename <- file.path(folder_name, "mbd.tree")

  create_razzo_params(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )
}
