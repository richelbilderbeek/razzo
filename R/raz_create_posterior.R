#' Create the posterior from parameters and an alignment.
#' @inheritParams default_params_doc
#' @return the posterior, as a list of these items:
#' \itemize{
#'   \item trees the posterior trees, as a \code{multiPhylo}
#'   \item estimates the posterior parameter estimates, as a \code{data.frame}
#'   \item \code{ns} the Nested Sampling information
#' }
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_posterior <- function(
  parameters,
  alignment
) {
  # Save the alignment to file
  fasta_filename <- tempfile(fileext = ".fasta")
  ape::write.FASTA(
    alignment,
    file = fasta_filename
  )

  sample_interval <- parameters$sample_interval
  crown_age <- parameters$age
  rng_seed <- parameters$seed
  chain_length <- parameters$chain_length
  sub_chain_length <- parameters$sub_chain_length

  # TODO: read site model from file
  site_model <- beautier::create_jc69_site_model() # Stub
  if ("site_model" %in% names(parameters)) {
    site_model <- parameters$site_model
    if (site_model == "JC69") {
      site_model <- beautier::create_jc69_site_model()
    } else {
      testit::assert(site_model == "GTR")
      site_model <- beautier::create_gtr_site_model()
    }
  }

  # TODO: read clock model from file
  clock_model <- beautier::create_strict_clock_model()
  if ("clock_model" %in% names(parameters)) {
    clock_model <- parameters$clock_model
    if (clock_model == "S") {
      clock_model <- beautier::create_strict_clock_model()
    } else {
      testit::assert(clock_model == "RLN")
      clock_model <- beautier::create_rln_clock_model()
    }
  }


  # Install maurices if needed
  try(mauricer::mrc_install("NS"), silent = TRUE)

  # Need the Linux executable
  # Windows executable won't work, as it does not load BEAST2 packages
  # Jar files won't work, as these does not load BEAST2 packages
  beast2_path <- beastier::get_default_beast2_bin_path()

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
    beast2_path = beast2_path
  )

  # Find the names that ends with 'trees' and replace it with trees
  trees_names_index <- which(!is.na(
    stringr::str_match(string = names(posterior), pattern = ".*_trees$")[, 1]
  ))
  names(posterior)[trees_names_index] <- "trees"
  posterior
}
