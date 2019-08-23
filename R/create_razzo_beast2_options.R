#' Create BEAST2 options that follow the razzo naming conventions
#' and article
#' @param rng_seed the RNG seed used in BEAST2 inference
#' @inheritParams default_params_doc
#' @export
create_razzo_beast2_options <- function(
  model_type,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {
  if (!model_type %in% c("generative", "candidate")) {
    stop("'model_type' must be either \"generative\" or \"candidate\"")
  }
  beast2_options <- NA
  if (model_type == "generative") {
    beast2_options <- beastier::create_beast2_options(
      input_filename = file.path(folder_name, "mbd_gen.xml"),
      output_log_filename = file.path(folder_name, "mbd_gen.log"),
      output_trees_filenames = file.path(folder_name, "mbd_gen.trees"),
      output_state_filename = file.path(folder_name, "mbd_gen.xml.state"),
      beast2_working_dir = peregrine::get_pff_tempfile(),
      rng_seed = rng_seed,
      overwrite = TRUE
    )
  } else {
    testit::assert(model_type == "candidate")
    beast2_options <- beastier::create_beast2_options(
      input_filename = file.path(folder_name, "mbd_best.xml"),
      output_log_filename = file.path(folder_name, "mbd_best.log"),
      output_trees_filenames = file.path(folder_name, "mbd_best.trees"),
      output_state_filename = file.path(folder_name, "mbd_best.xml.state"),
      beast2_working_dir = peregrine::get_pff_tempfile(),
      rng_seed = rng_seed,
      overwrite = TRUE
    )
  }
  testit::assert(!beautier::is_one_na(beast2_options))
  beast2_options
}