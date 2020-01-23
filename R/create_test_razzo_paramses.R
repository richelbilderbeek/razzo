#' Create a list of \code{razzo_params} to be used in testing
#' @inheritParams default_params_doc
#' @usage
#' create_test_razzo_paramses(
#'   project_folder_name = file.path(
#'     peregrine::get_pff_tempfile(), "razzo_project"
#'   )
#' )
#' @export
create_test_razzo_paramses <- function(
  project_folder_name = file.path(
    peregrine::get_pff_tempfile(), "razzo_project"
  )
) {
  mbd_paramses <- razzo::create_test_mbd_paramses()
  razzo_paramses <- razzo::create_razzo_paramses(
    project_folder_name = project_folder_name,
    mbd_paramses = mbd_paramses
  )
  for (i in seq_along(razzo_paramses)) {
    # Only take first 3 experiments
    razzo_paramses[[i]]$pir_params$experiments <-
      razzo_paramses[[i]]$pir_params$experiments[1:3]
    # Use simple inference models
    razzo_paramses[[i]]$pir_params$experiments[[2]]$inference_model$site_model <- beautier::create_hky_site_model() # nolint indeed long, sorry Demeter
    razzo_paramses[[i]]$pir_params$experiments[[3]]$inference_model$site_model <- beautier::create_tn93_site_model() # nolint indeed long, sorry Demeter
    razzo_paramses[[i]]$pir_params$experiments[[2]]$inference_model$clock_model <- beautier::create_strict_clock_model() # nolint indeed long, sorry Demeter
    razzo_paramses[[i]]$pir_params$experiments[[3]]$inference_model$clock_model <- beautier::create_strict_clock_model() # nolint indeed long, sorry Demeter
    razzo_paramses[[i]]$pir_params$experiments[[2]]$inference_model$tree_prior <- beautier::create_yule_tree_prior() # nolint indeed long, sorry Demeter
    razzo_paramses[[i]]$pir_params$experiments[[3]]$inference_model$tree_prior <- beautier::create_yule_tree_prior() # nolint indeed long, sorry Demeter
    for (j in seq_along(razzo_paramses[[i]]$pir_params$experiments)) {
      folder_name <- razzo::get_seed_folder_name(
        project_folder_name = project_folder_name,
        mbd_params = razzo_paramses[[i]]$mbd_params
      )
      experiment <- razzo_paramses[[i]]$pir_params$experiments[[j]]
      razzo_paramses[[i]]$pir_params$experiments[[j]]$inference_model$mcmc$chain_length <- 3000 # nolint sorry Demeter
      razzo_paramses[[i]]$pir_params$experiments[[j]]$inference_model$mcmc$store_every <- 1000 # nolint sorry Demeter
      razzo_paramses[[i]]$pir_params$experiments[[j]]$inference_model$mcmc$tracelog$filename <- # nolint sorry Demeter
        razzo::get_tracelog_filename(
          folder_name = folder_name,
          model_type = experiment$inference_conditions$model_type
        )
      razzo_paramses[[i]]$pir_params$experiments[[j]]$inference_model$mcmc$tracelog$log_every <- 1000 # nolint sorry Demeter
      razzo_paramses[[i]]$pir_params$experiments[[j]]$inference_model$mcmc$treelog$filename <- # nolint sorry Demeter
        razzo::get_treelog_filename(
          folder_name = folder_name,
          model_type = experiment$inference_conditions$model_type
        )
      razzo_paramses[[i]]$pir_params$experiments[[j]]$inference_model$mcmc$treelog$log_every <- 1000 # nolint sorry Demeter
    }
  }
  razzo_paramses
}
