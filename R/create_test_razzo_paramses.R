#' Create a list of \code{razzo_params} to be used in testing
#' @inheritParams default_params_doc
#' @export
create_test_razzo_paramses <- function(
  project_folder_name = file.path(
    peregrine::get_pff_tempfile(), "razzo_project"
  )
) {
  mbd_paramses <- create_test_mbd_paramses()
  razzo_paramses <- create_razzo_paramses(
    project_folder_name = project_folder_name,
    mbd_paramses = mbd_paramses
  )
  for (i in seq_along(razzo_paramses)) {
    for (j in seq_along(razzo_paramses[[i]]$pir_params$experiments)) {
      razzo_paramses[[i]]$pir_params$experiments[[j]]$inference_model$mcmc <-
        beautier::create_mcmc(chain_length = 3000, store_every = 1000)
    }
  }
  razzo_paramses
}
