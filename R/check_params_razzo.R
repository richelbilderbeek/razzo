#' Check if the \code{razzo_params} are valid razzo parameters.
#'
#' Will \link{stop} if the \code{razzo_params} are invalid,
#' else will do nothing
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
check_razzo_params <- function(
  razzo_params
) {
  check_razzo_params_names(razzo_params)

  razzo::check_mbd_params(razzo_params$mbd_params)
  razzo::check_misc_params(razzo_params$misc_params)
  pirouette::check_pir_params(pir_params = razzo_params$pir_params)
  peregrine::check_pff_pir_params(razzo_params$pir_params)

  if (!beautier::has_mrca_prior(
      razzo_params$pir_params$experiments[[1]]$inference_model
    )
  ) {
    "An inference model must have a valid MRCA prior"
  }
  pir_params <- razzo_params$pir_params
  if (beautier::is_one_na(pir_params$twinning_params)) {
    stop("'twinning_params' must have a valid non-NA value")
  }

  first_experiment <- pir_params$experiments[[1]]
  first_mrca_prior <- first_experiment$inference_model$mrca_prior
  if (beautier::is_one_na(first_mrca_prior)) {
    stop("An inference model must have a valid MRCA prior that is not NA")
  }
  if (razzo_params$mbd_params$crown_age !=
      first_mrca_prior$mrca_distr$mean$value
  ) {
    stop(
      "Crown ages in MBD param (", razzo_params$mbd_params$crown_age,
      ") and inference model's MRCA prior (",
      first_mrca_prior$mrca_distr$mean$value, ") must be equal"
    )
  }
  for (experiment in razzo_params$pir_params$experiments) {
    mrca_prior <- experiment$inference_model$mrca_prior
    if (beautier::is_one_na(mrca_prior)) {
      stop("Must use an MRCA prior")
    }
    if (beautier::is_one_na(mrca_prior$mrca_distr)) {
      stop("Must use an MRCA prior with a distribution")
    }
    if (beautier::is_one_na(mrca_prior$mrca_distr$mean)) {
      stop("Must use an MRCA prior with a distribution that has a mean")
    }
    if (beautier::is_one_na(mrca_prior$mrca_distr$mean$value)) {
      stop(
        "Must use an MRCA prior with a distribution that has ",
        "a mean with a value"
      )
    }
    if (length(mrca_prior$mrca_distr$mean$value) == 0) {
      stop(
        "Must use an MRCA prior with a distribution that has a mean ",
        "with a value"
      )
    }
    if (mrca_prior$mrca_distr$mean$value <= 0.0) {
      stop(
        "Must use an MRCA prior with a distribution that has a mean ",
        "with a non-zero and positive value"
      )
    }
  }
  gen_experiment <- razzo_params$pir_params$experiments[[1]]
  if (gen_experiment$inference_conditions$model_type != "generative") {
    stop(
      "razzo_params$pir_params$experiments[[1]]$inference_conditions$",
      "model_type' must be be 'generative'"
    )
  }
  cand_experiments <- razzo_params$pir_params$experiments[-1]
  for (cand_experiment in cand_experiments) {
    if (cand_experiment$inference_conditions$model_type != "candidate") {
      stop(
        "razzo_params$pir_params$experiments[[-1]]$inference_conditions$",
        "model_type' must all be 'candidate'"
      )
    }
  }
  # Filenames must be Peregrine-friendly and follow razzo convention
  razzo::check_razzo_params_filenames(razzo_params) # nolint razzo function
}

#' Check if the \code{razzo_params} is a list with elements
#' that have the required names to be valid razzo parameters.
#'
#' Will \link{stop} if not
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
check_razzo_params_names <- function(
  razzo_params
) {
  argument_names <- c(
    "mbd_params", "pir_params", "misc_params"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(razzo_params)) {
      stop(
        "'", arg_name, "' must be an element of a 'razzo_params'"
      )
    }
  }
}

