#' Create the parameters for all experiment
#' @inheritParams default_params_doc
#' @return a set of \code{razzo_params}
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_paramses <- function(
  project_folder_name,
  mbd_paramses = create_mbd_paramses(),
  error_measure_params = pirouette::create_error_measure_params(),
  mcmc_chain_length = beautier::create_mcmc()$chain_length
) {
  testit::assert(peregrine::is_pff(project_folder_name))
  data_folder_name <- "data"

  # Go through all biological parameters
  razzo_paramses <- list()
  # Must start at one, as the BEAST2 RNG seed must be at least one.
  index <- 1
  for (row_index in seq_along(mbd_paramses)) {
    # Extract the biological parameters and create a folder for them
    mbd_params <- mbd_paramses[[row_index]]
    parsettings_name <- paste0(
      mbd_params$lambda,
      "-",
      mbd_params$mu,
      "-",
      mbd_params$nu,
      "-",
      mbd_params$q
    )
    seed <- mbd_params$seed
    seed_folder <- file.path(
      project_folder_name,
      data_folder_name,
      parsettings_name,
      seed
    )
    # Cannot do model comparison on Windows
    pir_params <- create_razzo_pir_params(
      has_candidates = rappdirs::app_dir()$os != "win",
      has_twinning = TRUE,
      folder_name = seed_folder,
      rng_seed = seed
    )
    razzo_params <- razzo::create_razzo_params(
      mbd_params = mbd_params,
      pir_params = pir_params,
      misc_params = razzo::create_misc_params(seed_folder)
    )
    razzo_paramses[[index]] <- razzo_params
    index <- index + 1
  }
  razzo_paramses
}
