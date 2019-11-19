#' Create BEAST2 options that follow the razzo naming conventions
#' and article
#' @inheritParams default_params_doc
#' @param rng_seed the RNG seed used in BEAST2 inference
#' @inheritParams beastier::create_beast2_options
#' @export
create_razzo_beast2_options <- function(
  model_type,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {

  pirouette::check_model_type(model_type)
  beast2_options <- NA
  beast2_options <- beastier::create_beast2_options(
    input_filename = get_input_filename(
      folder_name = folder_name,
      model_type = model_type
    ),
    output_state_filename = get_output_state_filename(
      folder_name = folder_name,
      model_type = model_type
    ),
    rng_seed = rng_seed,
    overwrite = TRUE
  )
  testit::assert(!beautier::is_one_na(beast2_options))
  beast2_options
}
