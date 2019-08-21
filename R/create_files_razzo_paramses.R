#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @aliases create_parameters_files create_files_razzo_paramses
#' @export create_parameters_files create_files_razzo_paramses
create_parameters_files <- create_files_razzo_paramses <- function(
  project_folder_name = getwd(),
  experiment_type = "test"
) {
  testit::assert(peregrine::is_pff(project_folder_name))
  testit::assert(experiment_type == "test" || experiment_type == "full")
  if (experiment_type == "test") {
    n_replicates <- 2
    mbd_paramses <- create_mbd_params_table(
      lambda = 0.2,
      mu = 0.15,
      nu = 1.0,
      q = 0.1,
      n_replicates = n_replicates,
      crown_age = 6.0,
      cond = 1
    )
    testit::assert(
      nrow(unique(mbd_paramses)) == nrow(mbd_paramses)
    )
    parameters_filenames <- save_razzo_paramses(
      project_folder_name = project_folder_name,
      mbd_paramses = mbd_paramses,
      mcmc_chain_length = 3000
    )
    testit::assert(nrow(mbd_paramses) == length(parameters_filenames))
  } else {
    testit::assert(experiment_type == "full")
    n_replicates <- 2
    mbd_paramses <- create_mbd_params_table(
      lambda = 0.2,
      mu = c(0, 0.15),
      nu = c(1.0, 1.5, 2.0, 2.5),
      q = c(0.1, 0.15, 0.2),
      n_replicates = n_replicates,
      crown_age = 6.0,
      cond = 1
    )
    testit::assert(
      nrow(unique(mbd_paramses)) == nrow(mbd_paramses)
    )
    parameters_filenames <- save_razzo_paramses(
      project_folder_name = project_folder_name,
      mbd_paramses = mbd_paramses,
      mcmc_chain_length = 1111000
    )
    testit::assert(nrow(mbd_paramses) == length(parameters_filenames))
  }
  parameters_filenames
}

#' Create the parameter files according to the specified arguments
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
save_razzo_paramses <- function(
  project_folder_name,
  mbd_paramses = create_mbd_params_table(
    lambda = 0.2,
    mu = 0.15,
    nu = c(1.0, 1.5, 2.0),
    q = c(0.1, 0.15, 0.2),
    n_replicates = 2,
    crown_age = 15.0,
    cond = 1
  ),
  twinning_params = pirouette::create_twinning_params(
    twin_tree_filename = peregrine::get_pff_tempfile(),
    twin_alignment_filename = peregrine::get_pff_tempfile(),
    twin_evidence_filename = peregrine::get_pff_tempfile()
  ),
  alignment_params = pirouette::create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 1000),
    mutation_rate = 0.5 / unique(mbd_paramses$crown_age),
    fasta_filename = peregrine::get_pff_tempfile(
      pattern = "alignment_",
      fileext = ".fasta"
    )
  ),
  error_measure_params = pirouette::create_error_measure_params(),
  mcmc_chain_length = beautier::create_mcmc()$chain_length
) {
  testit::assert(peregrine::is_pff(project_folder_name))
  razzo_paramses <- create_razzo_paramses(
    project_folder_name = project_folder_name,
    mbd_paramses = mbd_paramses,
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    error_measure_params = error_measure_params,
    mcmc_chain_length = mcmc_chain_length
  )
  parameters_filenames <- rep(NA, length(razzo_paramses))
  for (i in seq_along(razzo_paramses)) {
    razzo_params <- razzo_paramses[[i]]
    # The parameters.RDa file must be saved in the same folder as the
    # FASTA filename (of the true alignment)
    fasta_filename <- razzo_params$pir_params$alignment_params$fasta_filename

    # Create the folder to store the parameters.RDa file in,
    # do not warn if it already exists
    folder_name <- dirname(fasta_filename)
    dir.create(folder_name, showWarnings = FALSE, recursive = TRUE)
    parameters_filename <- file.path(folder_name, "parameters.RDa")
    saveRDS(object = razzo_params, file = parameters_filename)
    testthat::expect_silent(
      check_razzo_params(readRDS(parameters_filename))
    )
    parameters_filenames[i] <- parameters_filename
  }
  parameters_filenames
}
