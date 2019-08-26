#' Check if all filenames in the \code{razzo_params}
#' follow the \code{razzo} naming convention
#' @noRd
check_razzo_params_filenames <- function() {
  folder_name <- dirname(
    razzo_params$pir_params$alignment_params$fasta_filename
  )
  gen_experiment <- razzo_params$pir_params$experiments[[1]]
  testit::assert(gen_experiment$inference_conditions$model_type == "generative")

  # True tree
  if (razzo_params$misc_params$tree_filename != file.path(folder_name, "mbd.tree")) { # nolint indeed long
    stop(
      "'razzo_params$misc_params$tree_filename' must be be '[folder_name]/mbd.tree'. \n", # nolint indeed long
      "Actual value: '", razzo_params$misc_params$tree_filename, "'\n",
      "[folder_name]: '", folder_name, "'\n",
      "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
    )
  }

  # BEAST2 input filename
  if (gen_experiment$beast2_options$input_filename != file.path(folder_name, "mbd_gen.xml")) { # nolint indeed long
    stop(
      "'razzo_params$pir_params$experiments[[1]]$beast2_options$input_filename' must be be '[folder_name]/mbd_gen.xml'. \n", # nolint indeed long
      "Actual value: '", gen_experiment$beast2_options$input_filename, "'\n", # nolint indeed long
      "[folder_name]: '", folder_name, "'\n",
      "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
    )
  }
  # BEAST2 output log filename
  if (gen_experiment$beast2_options$output_log_filename != file.path(folder_name, "mbd_gen.log")) { # nolint indeed long
    stop(
      "'razzo_params$pir_params$experiments[[1]]$beast2_options$output_log_filename' must be be '[folder_name]/mbd_gen.log'. \n", # nolint indeed long
      "Actual value: '", gen_experiment$beast2_options$output_log_filename, "'\n", # nolint indeed long
      "[folder_name]: '", folder_name, "'\n",
      "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
    )
  }
  # BEAST2 output trees filenames
  if (gen_experiment$beast2_options$output_trees_filenames != file.path(folder_name, "mbd_gen.trees")) { # nolint indeed long
    stop(
      "'razzo_params$pir_params$experiments[[1]]$beast2_options$output_trees_filenames' must be be '[folder_name]/mbd_gen.trees'. \n", # nolint indeed long
      "Actual value: '", gen_experiment$beast2_options$output_trees_filenames, "'\n", # nolint indeed long
      "[folder_name]: '", folder_name, "'\n",
      "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
    )
  }
  # BEAST2 output state filenames
  if (gen_experiment$beast2_options$output_state_filename != file.path(folder_name, "mbd_gen.xml.state")) { # nolint indeed long
    stop(
      "'razzo_params$pir_params$experiments[[1]]$beast2_options$output_state_filename' must be be '[folder_name]/mbd_gen.xml.state'. \n", # nolint indeed long
      "Actual value: '", gen_experiment$beast2_options$output_state_filename, "'\n", # nolint indeed long
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

  # True alignment
  if (razzo_params$pir_params$alignment_params$fasta_filename != file.path(folder_name, "mbd.fasta")) { # nolint indeed long
    stop(
      "'razzo_params$pir_params$alignment_params$fasta_filename' must be be '[folder_name]/mbd.fasta'. \n", # nolint indeed long
      "Actual value: '", razzo_params$pir_params$alignment_params$fasta_filename, "'\n", # nolint indeed long
      "[folder_name]: '", folder_name, "'\n",
      "(folder name taken from razzo_params$pir_params$alignment_params$fasta_filename)" # nolint indeed long
    )
  }
  # Twinning
  testit::assert(
    file.path(folder_name, "mbd_twin.tree") ==
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