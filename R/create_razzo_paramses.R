#' Create the parameters for all experiment
#' @inheritParams default_params_doc
#' @return a set of \code{razzo_params}
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_paramses <- function(
  project_folder_name,
  mbd_paramses = razzo::create_mbd_paramses(),
  error_measure_params = pirouette::create_error_measure_params()
) {
  testit::assert(peregrine::is_pff(project_folder_name))

  # Go through all biological parameters
  razzo_paramses <- list()
  # Must start at one, as the BEAST2 RNG seed must be at least one.
  index <- 1
  for (row_index in seq_along(mbd_paramses)) {
    # Extract the biological parameters and create a folder for them
    mbd_params <- mbd_paramses[[row_index]]
    seed_folder <- razzo::get_seed_folder_name(
      project_folder_name = project_folder_name,
      mbd_params = mbd_params
    )
    # Cannot do model comparison on Windows
    pir_params <- razzo::create_razzo_pir_params(
      has_candidates = rappdirs::app_dir()$os != "win",
      has_twinning = TRUE,
      folder_name = seed_folder,
      rng_seed = mbd_params$seed,
      error_measure_params = error_measure_params
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
