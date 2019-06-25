#' Create a \code{pir_params} with PFFs
#' @author Richel J.C. Bilderbeek
#' @export
create_test_pff_pir_params <- function() {
  pir_params <- pirouette::create_test_pir_params(
    alignment_params = create_test_alignment_params(
      fasta_filename = get_pff_tempfile()
    ),
    experiments = list(
      pirouette::create_test_gen_experiment(
        inference_model = beautier::create_inference_model(
          mrca_prior = beautier::create_mrca_prior(
            mrca_distr = beautier::create_normal_distr(
              mean = 15.0,
              sigma = 0.001
            ),
            is_monophyletic = TRUE
          ),
          mcmc = beautier::create_mcmc(
            chain_length = 2000, store_every = 1000
          )
        ),
        beast2_options = beastier::create_beast2_options(
          input_filename = get_pff_tempfile(),
          output_log_filename = get_pff_tempfile(),
          output_trees_filenames = get_pff_tempfile(),
          output_state_filename = get_pff_tempfile(),
          beast2_working_dir = get_pff_tempdir()
        )
      )
    ),
    twinning_params = pirouette::create_twinning_params(
      twin_tree_filename = get_pff_tempfile(),
      twin_alignment_filename = get_pff_tempfile(),
      twin_evidence_filename = get_pff_tempfile(
        pattern = "evidence_",
        fileext = ".csv"
      )
    ),
    evidence_filename = get_pff_tempfile(
      pattern = "evidence_",
      fileext = ".csv"
    )
  )
  pir_params
}
