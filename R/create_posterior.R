#' Create the posterior from parameters and an alignment.
#' @inheritParams default_params_doc
#' @return the posterior, as a list of these items:
#' \itemize{
#'   \item trees the posterior trees, as a \code{multiPhylo}
#'   \item estimates the posterior parameter estimates, as a \code{data.frame}
#'   \item \code{ns} the Nested Sampling information
#' }
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_posterior <- function(
  parameters,
  alignment
) {
  testit::assert(beastier::is_beast2_installed())

  # Save the alignment to file
  fasta_filename <- tempfile(fileext = ".fasta")
  ape::write.FASTA(
    alignment,
    file = fasta_filename
  )

  sample_interval <- parameters$sample_interval
  crown_age <- parameters$crown_age
  rng_seed <- parameters$seed
  chain_length <- parameters$chain_length
  clock_model <- parameters$clock_model
  site_model <- parameters$site_model

  testit::assert(!is.null(sample_interval))
  testit::assert(!is.null(crown_age))
  testit::assert(!is.null(rng_seed))
  testit::assert(!is.null(chain_length))
  testit::assert(!is.null(clock_model))
  testit::assert(!is.null(site_model))

  if (!(clock_model %in% get_clock_models())) { # nolint internal function
    stop(
      "'clock_model' must be among the following: ",
      paste(get_clock_models(), collapse = ", ") # nolint internal function
    )
  }
  if (clock_model == "strict") {
    clock_model_function <- beautier::create_clock_model_strict
  }
  if (clock_model == "rln") {
    clock_model_function <- beautier::create_clock_model_rln
  }

  if (!(site_model %in% get_site_models())) { # nolint internal function
    stop("'site_model' must be among the following: ",
         paste(get_site_models(), collapse = ", ") # nolint internal function
    )
  }
  # Up the site model from a character vector to a data structure
  if (site_model == "jc69") {
    site_model <- beautier::create_jc69_site_model()
  } else {
    testit::assert(site_model == "gtr")
    site_model <- beautier::create_gtr_site_model()
  }

  posterior <- babette::bbt_run(
    fasta_filename = fasta_filename,
    mcmc = beautier::create_mcmc(
      chain_length = chain_length,
      store_every = sample_interval
    ),
    site_models = site_model,
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
    rng_seed = rng_seed
  )

  # Find the names that ends with 'trees' and replace it with trees
  trees_names_index <- which(!is.na(
    stringr::str_match(string = names(posterior), pattern = ".*_trees$")[, 1]
  ))
  names(posterior)[trees_names_index] <- "trees"
  posterior
}
