#' Check if the \code{razzo_params} are valid razzo parameters.
#'
#' Will \link{stop} if the \code{razzo_params} are invalid,
#' else will do nothing
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @aliases check_razzo_params check_params_razzo
#' @export check_razzo_params check_params_razzo
check_razzo_params <- check_params_razzo <- function(
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
  peregrine::check_pff_pir_params(razzo_params$pir_params)

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

  if (1 == 2) {
    # Filenames
    folder_name <- dirname(
      razzo_params$pir_params$alignment_params$fasta_filename
    )
    # True tree
    if (razzo_params$misc_params$tree_filename != file.path(folder_name, "mbd.tree")) { # nolint indeed long
      stop(
        "'razzo_params$misc_params$tree_filename' must be be '[folder_name]/mbd.tree'. \n", # nolint indeed long
        "Actual value: '", razzo_params$misc_params$tree_filename, "'\n",
        "[folder_name]: '", folder_name, "'\n",
        "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
      )
    }
    # True alignment
    if (razzo_params$pir_params$alignment_params$fasta_filename != file.path(folder_name, "pbd.fasta")) { # nolint indeed long
      stop(
        "'razzo_params$pir_params$alignment_params$fasta_filename' must be be '[folder_name]/pbd.fasta'. \n", # nolint indeed long
        "Actual value: '", razzo_params$pir_params$alignment_params$fasta_filename, "'\n",
        "[folder_name]: '", folder_name, "'\n",
        "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
      )
    }

    # First experiment must be generative
    gen_experiment <- razzo_params$pir_params$experiments[[1]]
    if (gen_experiment$inference_conditions$model_type != "generative") { # nolint indeed long
      stop(
        "'razzo_params$pir_params$experiments[[1]]$inference_conditions$model_type' must be be 'generative'. \n", # nolint indeed long
        "Actual value: '", gen_experiment$inference_conditions$model_type, "'\n"
      )
    }

    # BEAST2 input filename
    if (razzo_params$pir_params$experiments[[1]]$beast2_options$input_filename != file.path(folder_name, "mbd_gen.xml")) { # nolint indeed long
      stop(
        "'razzo_params$pir_params$experiments[[1]]$beast2_options$input_filename' must be be '[folder_name]/mbd_gen.xml'. \n", # nolint indeed long
        "Actual value: '", razzo_params$pir_params$experiments[[1]]$beast2_options$input_filename, "'\n", # nolint indeed long
        "[folder_name]: '", folder_name, "'\n",
        "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
      )
    }
    # BEAST2 output log filename
    if (razzo_params$pir_params$experiments[[1]]$beast2_options$output_log_filename != file.path(folder_name, "mbd_gen.log")) { # nolint indeed long
      stop(
        "'razzo_params$pir_params$experiments[[1]]$beast2_options$output_log_filename' must be be '[folder_name]/mbd_gen.log'. \n", # nolint indeed long
        "Actual value: '", razzo_params$pir_params$experiments[[1]]$beast2_options$output_log_filename, "'\n", # nolint indeed long
        "[folder_name]: '", folder_name, "'\n",
        "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
      )
    }
    # BEAST2 output trees filenames
    if (razzo_params$pir_params$experiments[[1]]$beast2_options$output_trees_filenames != file.path(folder_name, "mbd_gen.trees")) { # nolint indeed long
      stop(
        "'razzo_params$pir_params$experiments[[1]]$beast2_options$output_trees_filenames' must be be '[folder_name]/mbd_gen.trees'. \n", # nolint indeed long
        "Actual value: '", razzo_params$pir_params$experiments[[1]]$beast2_options$output_trees_filenames, "'\n", # nolint indeed long
        "[folder_name]: '", folder_name, "'\n",
        "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
      )
    }
    # BEAST2 output state filenames
    if (razzo_params$pir_params$experiments[[1]]$beast2_options$output_state_filename != file.path(folder_name, "mbd_gen.xml.state")) { # nolint indeed long
      stop(
        "'razzo_params$pir_params$experiments[[1]]$beast2_options$output_state_filename' must be be '[folder_name]/mbd_gen.xml.state'. \n", # nolint indeed long
        "Actual value: '", razzo_params$pir_params$experiments[[1]]$beast2_options$output_state_filename, "'\n", # nolint indeed long
        "[folder_name]: '", folder_name, "'\n",
        "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
      )
    }
    # Lost inspiration for this grunt work ...
    cand_experiments <- razzo_params$pir_params$experiments[-1]
    for (cand_experiment in cand_experiments) {
      testit::assert(
        cand_experiment$inference_conditions$model_type ==
        "candidate"
      )
      testit::assert(
        file.path(folder_name, "mbd_best.xml") ==
        cand_experiment$beast2_options$input_filename
      )
      testit::assert(
        file.path(folder_name, "mbd_best.log") ==
        cand_experiment$beast2_options$output_log_filename
      )
      testit::assert(
        file.path(folder_name, "mbd_best.trees") ==
        cand_experiment$beast2_options$output_trees_filenames
      )
      testit::assert(
        file.path(folder_name, "mbd_best.xml.state") ==
        cand_experiment$beast2_options$output_state_filename
      )
      testit::assert(
        file.path(folder_name, "mbd_nltts_best.csv") ==
        cand_experiment$errors_filename
      )
    }
    # Twinning
    testit::assert(
      file.path(folder_name, "mbd_twin.newick") ==
      razzo_params$pir_params$twinning_params$twin_tree_filename
    )
    testit::assert(
      file.path(folder_name, "mbd_twin.fasta") ==
      razzo_params$pir_params$twinning_params$twin_alignment_filename
    )
    testit::assert(
      file.path(folder_name, "mbd_marg_lik_twin.csv") ==
      razzo_params$pir_params$twinning_params$twin_evidence_filename
    )
    testit::assert(
      file.path(folder_name, "mbd_marg_lik.csv") ==
      razzo_params$pir_params$evidence_filename
    )
  }
}
