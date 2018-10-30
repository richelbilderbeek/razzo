#' Creates a list of parameters,
#' after checking the arguments for their validity
#' @inheritParams default_params_doc
#' @return a list of parameters
#' @export
#' @author Richel J.C. Bilderbeek
raz_create_params <- function(
  lambda = 0.1,
  mu = 0.05,
  nu = 1,
  q = 0.1,
  seed = 42,
  crown_age = 15,
  sequence_length = 100,
  mbd_mutation_rate = 1.0 / crown_age,
  bd_mutation_rate = 1.0 / crown_age,
  sample_interval = 1000,
  chain_length = 2000,
  sub_chain_length = 500,
  clock_model = "strict",
  site_model = "jc69"
) {
  if (lambda < 0.0) {
    stop("'lambda' must be positive")
  }
  if (mu < 0.0) {
    stop("mu has to be non negative!")
  }
  if (nu <= 0.0) {
    stop("nu has to be non negative!")
  }
  if (q < 0.0 || q > 1.0) {
    stop("q has to be between zero and one!")
  }
  if (!is.numeric(seed)) {
    stop("seed must be an integer!")
  }
  if (crown_age < 0) {
    stop("age has to be non negative!")
  }
  if (floor(sequence_length) != ceiling(sequence_length) ||
      sequence_length < 0
  ) {
    stop("sequence_length has to be a positive integer number!")
  }
  if (mbd_mutation_rate < 0.0) {
    stop("mbd_mutation_rate has to be a positive number!")
  }
  if (bd_mutation_rate < 0.0) {
    stop("bd_mutation_rate has to be a positive number!")
  }
  if (sample_interval <= 0) {
    stop("sample_interval has to be a positive number!")
  }
  if (chain_length <= 0) {
    stop("chain_length has to be a positive number!")
  }
  if (sub_chain_length <= 0) {
    stop("sub_chain_length has to be a positive number!")
  }
  if (!(clock_model == "strict" || clock_model == "rln")) {
    stop("'clock_model' must be either 'strict' or 'rln'")
  }
  if (!(site_model == "jc69" || site_model == "gtr")) {
    stop("'site_model' must be either 'jc69' or 'gtr'")
  }

  params <- list(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    seed = seed,
    crown_age = crown_age,
    sequence_length = sequence_length,
    mbd_mutation_rate = mbd_mutation_rate,
    bd_mutation_rate = bd_mutation_rate,
    sample_interval = sample_interval,
    chain_length = chain_length,
    sub_chain_length = sub_chain_length,
    clock_model = clock_model,
    site_model = site_model
  )
  params
}
