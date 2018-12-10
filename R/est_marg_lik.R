#' Estimates the marginal likelihood of a model
#' (as specified in \code{parameters})
#' for an alignment.
#' @inheritParams default_params_doc
#' @return a list with the following elements:
#' \itemize{
#'   \item marg_log_lik the log of the estimated marginal likelihood
#'   \item marg_log_lik_sd the ?log of the error in that estimation
#' }
#' @author Richel J.C. Bilderbeek
#' @export
est_marg_lik <- function(
  parameters,
  alignment
) {
  if (!(parameters$clock_model %in% get_clock_models())) { # nolint internal function
    stop(
      "'clock_model' must be among the following: ",
      paste(get_clock_models(), collapse = ", ") # nolint internal function
    )
  }
  if (!(parameters$site_model %in% get_site_models())) { # nolint internal function
    stop("'site_model' must be among the following: ",
         paste(get_site_models(), collapse = ", ") # nolint internal function
    )
  }

  testit::assert(beastier::is_beast2_installed())
  testit::assert(mauricer::mrc_is_installed("NS"))
  testit::assert(rappdirs::app_dir()$os != "win")

  # Save the alignment to file
  fasta_filename <- tempfile(fileext = ".fasta")
  ape::write.FASTA(
    alignment,
    file = fasta_filename
  )

  crown_age <- parameters$crown_age
  rng_seed <- parameters$seed
  chain_length <- parameters$chain_length
  sub_chain_length <- parameters$sub_chain_length
  clock_model <- parameters$clock_model
  site_model <- parameters$site_model


  # for the full experiment, we use 100 active points, sampling every 10k
  # for the test, we use 3 active points, sampling every 1k
  sample_interval <- max(1000, chain_length / 100)

  testit::assert(!is.null(sample_interval))
  testit::assert(!is.null(crown_age))
  testit::assert(!is.null(rng_seed))
  testit::assert(!is.null(chain_length))
  testit::assert(!is.null(sub_chain_length))
  testit::assert(!is.null(clock_model))
  testit::assert(!is.null(site_model))

  if (clock_model == "strict") {
    clock_model_function <- beautier::create_clock_model_strict
  }
  if (clock_model == "rln") {
    clock_model_function <- beautier::create_clock_model_rln
  }

  # Up the site model from a character vector to a data structure
  if (site_model == "jc69") {
    site_model <- beautier::create_jc69_site_model()
  } else {
    testit::assert(site_model == "gtr")
    site_model <- beautier::create_gtr_site_model()
  }

  ns <- babette::bbt_run(
    fasta_filename = fasta_filename,
    mcmc = beautier::create_mcmc_nested_sampling(
      chain_length = chain_length,
      store_every = sample_interval,
      particle_count = 1, # 1 particle
      sub_chain_length = sub_chain_length,
      epsilon = 1e-13 # epsilon
    ),
    site_model = site_model,
    clock_model = clock_model_function(),
    tree_prior = beautier::create_bd_tree_prior(),
    mrca_prior = beautier::create_mrca_prior(
      alignment_id = beautier::get_alignment_id(fasta_filename),
      taxa_names = beautier::get_taxa_names(fasta_filename),
      is_monophyletic = TRUE,
      mrca_distr = beautier::create_normal_distr(
        mean  = beautier::create_mean_param(value = crown_age),
        sigma = beautier::create_sigma_param(value = 0.001)
      )
    ),
    rng_seed = rng_seed,
    beast2_path = beastier::get_default_beast2_bin_path()
  )$ns

  list(
    marg_log_lik = ns$marg_log_lik,
    marg_log_lik_sd = ns$marg_log_lik_sd
  )
}
