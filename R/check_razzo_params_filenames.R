#' Check if all filenames in the \code{razzo_params}
#' follow the \code{razzo} naming convention
#' @noRd
check_razzo_params_filenames <- function(razzo_params) {
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
      get_input_filename(
        folder_name = folder_name,
        model_type = "candidate"
      ) ==
      cand_experiment$beast2_options$input_filename
    )
    testit::assert(
      get_output_log_filenames(
        folder_name = folder_name,
        model_type = "candidate"
      ) ==
      cand_experiment$beast2_options$output_log_filename
    )
    testit::assert(
      get_output_trees_filenames(
        folder_name = folder_name,
        model_type = "candidate"
      ) ==
      cand_experiment$beast2_options$output_trees_filenames
    )
    testit::assert(
      get_output_state_filename(
        folder_name = folder_name,
        model_type = "candidate"
      ) ==
        cand_experiment$beast2_options$output_state_filename
    )
    testit::assert(
      get_errors_filename(
        folder_name = folder_name,
        model_type = "candidate"
      ) ==
      cand_experiment$errors_filename
    )
  }

  # True alignment
  if (
    razzo_params$pir_params$alignment_params$fasta_filename !=
    get_alignment_filename(folder_name = folder_name, tree_type = "true")
  ) { # nolint indeed long
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
    get_alignment_filename(folder_name = folder_name, tree_type = "twin") ==
    razzo_params$pir_params$twinning_params$twin_alignment_filename
  )
  testit::assert(
    get_evidence_filename(folder_name = folder_name, tree_type = "twin") ==
    razzo_params$pir_params$twinning_params$twin_evidence_filename
  )
  testit::assert(
    get_evidence_filename(folder_name = folder_name, tree_type = "true") ==
    razzo_params$pir_params$evidence_filename
  )

}
