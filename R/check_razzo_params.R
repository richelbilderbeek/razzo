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

  check_mbd_params(razzo_params$mbd_params)
  check_misc_params(razzo_params$misc_params)
  pirouette::check_pir_params(razzo_params$pir_params)
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
        "Must use an MRCA prior with a distribution that has a mean with a value"
      )
    }
    if (length(mrca_prior$mrca_distr$mean$value) == 0) {
      stop(
        "Must use an MRCA prior with a distribution that has a mean with a value"
      )
    }
    if (mrca_prior$mrca_distr$mean$value <= 0.0) {
      stop(
        "Must use an MRCA prior with a distribution that has a mean ",
        "with a non-zero and positive value"
      )
    }
  }

  if (!is_pff(pir_params$twinning_params$twin_tree_filename)) {
    stop("Peregrine-unfriendly filename for '")
  }
  if (!is_pff(pir_params$twinning_params$twin_alignment_filename)) {
    stop("Peregrine-unfriendly filename for '")
  }
  if (!is_pff(pir_params$twinning_params$twin_evidence_filename)) {
    stop("Peregrine-unfriendly filename for '")
  }
  if (!is_pff(pir_params$alignment_params$fasta_filename)) {
    stop("Peregrine-unfriendly filename for '")
  }
  for (i in seq_along(pir_params$experiments)) {
    experiment <- pir_params$experiments[[i]]
    if (!is_pff(experiment$beast2_options$input_filename)) {
      stop("Peregrine-unfriendly filename for '")
    }
    if (!is_pff(experiment$beast2_options$output_log_filename)) {
      stop("Peregrine-unfriendly filename for '")
    }
    if (!is_pff(experiment$beast2_options$output_trees_filenames)) {
      stop("Peregrine-unfriendly filename for '")
    }
    if (!is_pff(experiment$beast2_options$output_state_filename)) {
      stop("Peregrine-unfriendly filename for '")
    }
    if (!is_pff(experiment$beast2_options$beast2_working_dir)) {
      stop("Peregrine-unfriendly filename for '")
    }
    if (!is_pff(experiment$beast2_options$beast2_path)) {
      stop("Peregrine-unfriendly filename for '")
    }
  }

}
