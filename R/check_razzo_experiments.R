#' Check razzo experiments
#' @inheritParams default_params_doc
#' @export
check_razzo_experiments <- function(experiments) {
  pirouette::check_experiments(experiments)

  for (experiment in experiments) {
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
}
