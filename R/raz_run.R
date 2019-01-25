#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_run <- function(
  razzo_params
) {
  check_razzo_params(razzo_params) # nolint razzo function
  testit::assert(beastier::is_beast2_installed())

  chain_length <- razzo_params$mcmc_chain_length
  store_every <- razzo_params$mcmc_store_every
  testit::assert(chain_length >= 1000)
  testit::assert(store_every >= 1000)

  set.seed(razzo_params$tree_sim_rng_seed)

  # Note: if speciation rates are zero, PBD::pbd_sim will last forever
  pbd_output <- becosys::bco_pbd_sim(
    pbd_params = razzo_params$pbd_params,
    crown_age = -12345678901234567890
  )

  true_phylogeny <- NA
  if (razzo_params$sampling_method == "shortest") {
    true_phylogeny <- pbd_output$stree_shortest
  } else if (razzo_params$sampling_method == "longest") {
    true_phylogeny <- pbd_output$stree_longest
  } else {
    testit::assert(razzo_params$sampling_method == "random")
    true_phylogeny <- pbd_output$stree_random
  }

  site_model <- NA
  if (razzo_params$site_model == "JC69") {
    site_model <- beautier::create_jc69_site_model()
  } else {
    testit::assert(razzo_params$site_model == "GTR")
    site_model <- beautier::create_gtr_site_model()
  }
  testit::assert(beautier::is_site_model(site_model))

  clock_model <- NA
  if (razzo_params$clock_model == "strict") {
    clock_model <- beautier::create_strict_clock_model()
  } else {
    testit::assert(razzo_params$clock_model == "RLN")
    clock_model <- beautier::create_rln_clock_model()
  }
  testit::assert(beautier::is_clock_model(clock_model))

  out <- pirouette::pir_run(
    phylogeny = true_phylogeny,
    alignment_params = razzo_params$alignment_params,
    twinning_params = ,
    model_select_params = ,
    inference_param = ,
    error_measure_params = create_error_measure_params()
  )
  out$razzo_params <- razzo_params
  out$incipient_tree <- pbd_output$igtree.extant
  out$species_tree <- true_phylogeny
  out
}
