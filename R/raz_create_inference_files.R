#' Create the inference files from a FASTA file.
#' For example, for a FASTA file named '/my_folder/mbd.fasta', this
#' function will create:
#' \itemize{
#'   \item '/my_folder/mbd.trees' BEAST2 posterior trees
#'   \item '/my_folder/mbd.log' BEAST2 parameter estimates
#'   \item '/my_folder/mbd_mar_lik.csv' BEAST2 marginal likelihood estimate
#' }
#' Assumes, for a FASTA file named '/my_folder/mbd.fasta', this
#'   that there is a parameters file named '/my_folder/parameters.csv'
#' @inheritParams default_params_doc
#' @return names of the files created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_inference_files <- function(
  fasta_filename
)
{

  testit::assert(file.exists(fasta_filename))

  parameters_filename <- file.path(dirname(fasta_filename), "parameters.csv")
  testit::assert(file.exists(parameters_filename))

  # Read the parameters
  parameters <- razzo::raz_open_parameters_file(parameters_filename)
  testit::assert(parameters$lambda >= 0.0)

  base <- tools::file_path_sans_ext(fasta_filename)

  trees_filename <- paste0(c(base, ".trees"), collapse = "")
  log_filename <- paste0(c(base, ".log"), collapse = "")
  mar_lik_filename <- paste0(c(base, "_mar_lik.csv"), collapse = "")

  # TODO: read from parameter file, or set to 1000
  sample_interval <- parameters$sample_interval
  crown_age <- parameters$age
  rng_seed <- parameters$seed
  chain_length <- parameters$chain_length

  # The Nested Sampling subchain length
  sub_chain_length <- parameters$sub_chain_length


  # TODO: read site model from file
  site_model <- beautier::create_jc69_site_model() # Stub
  if (1 == 2) {
    site_model <- NULL
    if (site_model == "JC69") {
      site_model <- beautier::create_jc69_site_model()
    } else {
      testit::assert(site_model == "GTR")
      site_model <- beautier::create_gtr_site_model()
    }
  }

  # TODO: read clock model from file
  clock_model <- beautier::create_strict_clock_model()
  if (1 == 2) {
    clock_model <- NULL
    if (clock_model == "S") {
      clock_model <- beautier::create_strict_clock_model()
    } else {
      testit::assert(clock_model == "RLN")
      clock_model <- beautier::create_rln_clock_model()
    }
  }

  # TODO: create the BEAST2 posterior trees, parameter estimates
  # and marginal likelihood files
  if (1 == 2) {
    try(mauricer::mrc_install("NS"), silent = TRUE)

    # What is input_filename ???

    posterior <- babette::bbt_run(
      fasta_filenames = fasta_filename,
      mcmc = beautier::create_mcmc_nested_sampling(
        chain_length = chain_length,
        store_every = sample_interval,
        sub_chain_length = sub_chain_length
      ),
      site_models = site_model,
      clock_models = clock_model,
      tree_priors = beautier::create_bd_tree_prior(),
      mrca_priors = beautier::create_mrca_prior(
        alignment_id = beautier::get_alignment_id(fasta_filename),
        taxa_names = beautier::get_taxa_names(fasta_filename),
        is_monophyletic = TRUE,
        mrca_distr = beautier::create_normal_distr(
          mean  = beautier::create_mean_param(value = crown_age),
          sigma = beautier::create_sigma_param(value = 0.001)
        )
      ),
      rng_seed = rng_seed,
      beast2_output_trees_filenames = trees_filename, # Will create it
      beast2_output_log_filename = log_filename, # Will create it
      verbose = FALSE
    )

    # Save marginal likelihood data
    utils::write.csv(x = posterior$ns, file = mar_lik_filename)
  }

  inference_filenames <- c(
    trees_filename,
    log_filename,
    mar_lik_filename
  )
  inference_filenames
}
