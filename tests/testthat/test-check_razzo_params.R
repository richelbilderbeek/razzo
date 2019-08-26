test_that("use", {

  good_razzo_params <- create_test_razzo_params()
  expect_silent(
    check_razzo_params(razzo_params = good_razzo_params)
  )

  # Check elements
  razzo_params <- good_razzo_params
  razzo_params$mbd_params <- NULL
  expect_error(
    check_razzo_params(razzo_params),
    "'mbd_params' must be an element of a 'razzo_params'"
  )

  razzo_params <- good_razzo_params
  razzo_params$pir_params <- NULL
  expect_error(
    check_razzo_params(razzo_params),
    "'pir_params' must be an element of a 'razzo_params'"
  )

  razzo_params <- good_razzo_params
  razzo_params$misc_params <- NULL
  expect_error(
    check_razzo_params(razzo_params),
    "'misc_params' must be an element of a 'razzo_params'"
  )
})


test_that("check filenames", {

  good_razzo_params <- create_test_razzo_params(
    pir_params = create_test_razzo_pir_params(
      has_candidates = TRUE,
      has_twinning = TRUE
    )
  )
  expect_silent(check_razzo_params(good_razzo_params))

  # True alignment
  # One may expect the error:
  #   "'razzo_params$pir_params$alignment_params$fasta_filename' must be be '\\[folder_name\\]/pbd.fasta'" # nolint indeed long
  # but this is false, as the folder of the alignment is used to generate
  # all other errors
  razzo_params <- good_razzo_params
  razzo_params$pir_params$alignment_params$fasta_filename <- "nonsense"
  expect_error(
    check_razzo_params(razzo_params),
      "'razzo_params\\$misc_params\\$tree_filename' must be be '\\[folder_name\\]/mbd.tree'", # nolint indeed long
  )
  # First experiment must be generative
  # (yes, to test this is hard to set up)
  razzo_params <- good_razzo_params
  razzo_params$pir_params$experiments[[1]]$inference_conditions$model_type <-
    "candidate"
  razzo_params$pir_params$experiments[[1]]$inference_conditions$run_if <-
    "best_candidate"
  razzo_params$pir_params$experiments[[1]]$inference_conditions$do_measure_evidence <-  # nolint sorry Demeter
    TRUE
  razzo_params$pir_params$experiments[[1]]$errors_filename <-
    razzo_params$pir_params$experiments[[2]]$errors_filename
  razzo_params$pir_params$experiments[[1]]$beast2_options <-
    razzo_params$pir_params$experiments[[2]]$beast2_options
  expect_error(
    check_razzo_params(razzo_params),
    "razzo_params\\$pir_params\\$experiments\\[\\[1\\]\\]\\$inference_conditions\\$model_type' must be be 'generative'" # nolint indeed long
  )
  # BEAST2 input filename
  razzo_params <- good_razzo_params
  razzo_params$pir_params$experiments[[1]]$beast2_options$input_filename <-
    "nonsense"
  expect_error(
    check_razzo_params(razzo_params),
    "'razzo_params\\$pir_params\\$experiments\\[\\[1\\]\\]\\$beast2_options\\$input_filename' must be be '\\[folder_name\\]/mbd_gen.xml'" # nolint indeed long
  )
  # BEAST2 output log filename
  razzo_params <- good_razzo_params
  razzo_params$pir_params$experiments[[1]]$beast2_options$output_log_filename <-
    "nonsense"
  expect_error(
    check_razzo_params(razzo_params),
    "'razzo_params\\$pir_params\\$experiments\\[\\[1\\]\\]\\$beast2_options\\$output_log_filename' must be be '\\[folder_name\\]/mbd_gen.log'" # nolint indeed long
  )
  # BEAST2 output trees filename
  razzo_params <- good_razzo_params
  razzo_params$pir_params$experiments[[1]]$beast2_options$output_trees_filenames <-  # nolint sorry Demeter
    "nonsense"
  expect_error(
    check_razzo_params(razzo_params),
    "'razzo_params\\$pir_params\\$experiments\\[\\[1\\]\\]\\$beast2_options\\$output_trees_filenames' must be be '\\[folder_name\\]/mbd_gen.trees'" # nolint indeed long
  )
  # BEAST2 output trees filename
  razzo_params <- good_razzo_params
  razzo_params$pir_params$experiments[[1]]$beast2_options$output_state_filename <-  # nolint sorry Demeter
    "nonsense"
  expect_error(
    check_razzo_params(razzo_params),
    "'razzo_params\\$pir_params\\$experiments\\[\\[1\\]\\]\\$beast2_options\\$output_state_filename' must be be '\\[folder_name\\]/mbd_gen.xml.state'" # nolint indeed long
  )
  for (i in seq(2, 3)) {
    # First experiment must be candidate
    # (yes, to test this is hard to set up)
    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$inference_conditions$model_type <-
      "generative"
    razzo_params$pir_params$experiments[[i]]$inference_conditions$run_if <-
      "always"
    razzo_params$pir_params$experiments[[i]]$inference_conditions$do_measure_evidence <-  # nolint sorry Demeter
      FALSE
    razzo_params$pir_params$experiments[[i]]$errors_filename <-
      razzo_params$pir_params$experiments[[2]]$errors_filename
    razzo_params$pir_params$experiments[[i]]$beast2_options <-
      razzo_params$pir_params$experiments[[2]]$beast2_options
    expect_error(
      check_razzo_params(razzo_params),
      "Specifying more than one 'generative' model experiment is redundant"
    )
    # BEAST2 input file
    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$input_filename <-
      "nonsense"
    expect_error(
      check_razzo_params(razzo_params)
    )
    # BEAST2 output log file
    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[2]]$beast2_options$output_log_filename <-  # nolint sorry Demeter
      "nonsense"
    razzo_params$pir_params$experiments[[3]]$beast2_options$output_log_filename <-  # nolint sorry Demeter
      "nonsense"
    expect_error(
      check_razzo_params(razzo_params)
    )
    # BEAST2 ouput trees files
    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$output_trees_filenames <-  # nolint sorry Demeter
      "nonsense"
    expect_error(
      check_razzo_params(razzo_params)
    )
    # BEAST2 input file
    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$output_state_filename <- # nolint sorry Demeter
      "nonsense"
    expect_error(
      check_razzo_params(razzo_params)
    )

    # Errors file
    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$errors_filename <- "nonsense"
    expect_error(
      check_razzo_params(razzo_params)
    )
  }
  # Twinning params: tree
  razzo_params <- good_razzo_params
  razzo_params$pir_params$twinning_params$twin_tree_filename <- "nonsense"
  expect_error(
    check_razzo_params(razzo_params)
  )
  # Twinning params: alignment
  razzo_params <- good_razzo_params
  razzo_params$pir_params$twinning_params$twin_alignment_filename <- "nonsense"
  expect_error(
    check_razzo_params(razzo_params)
  )
  # Twinning params: evidence
  razzo_params <- good_razzo_params
  razzo_params$pir_params$twinning_params$twin_evidence_filename <- "nonsense"
  expect_error(
    check_razzo_params(razzo_params)
  )
  # Evidence
  razzo_params <- good_razzo_params
  razzo_params$pir_params$evidence_filename <- "nonsense"
  expect_error(
    check_razzo_params(razzo_params)
  )


  # misc params
  razzo_params <- good_razzo_params
  razzo_params$misc_params$tree_filename <- NA
  expect_error(
    check_razzo_params(razzo_params),
    "misc_params\\$tree_filename is not of class 'character'"
  )

  razzo_params <- good_razzo_params
  razzo_params$misc_params$tree_filename <- "/no_way.newick"
  expect_error(
    check_razzo_params(razzo_params),
    "'misc_params\\$tree_filename' must be Peregrine-friendly"
  )

  # misc_params
  razzo_params <- good_razzo_params
  razzo_params$misc_params$tree_filename <- "nonsense"
  expect_error(
    check_razzo_params(razzo_params),
    "'misc_params\\$tree_filename' must end with 'mbd.tree'"
  )
})
