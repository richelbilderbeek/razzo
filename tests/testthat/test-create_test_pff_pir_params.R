test_that("use", {
  pir_params <- create_test_pff_pir_params()
  expect_true(is_pff(pir_params$twinning_params$twin_tree_filename))
  expect_true(is_pff(pir_params$twinning_params$twin_alignment_filename))
  expect_true(is_pff(pir_params$twinning_params$twin_evidence_filename))
  expect_true(is_pff(pir_params$alignment_params$fasta_filename))
  for (experiment in pir_params$experiments) {
    expect_true(is_pff(experiment$beast2_options$input_filename))
    expect_true(is_pff(experiment$beast2_options$output_log_filename))
    expect_true(is_pff(experiment$beast2_options$output_trees_filenames))
    expect_true(is_pff(experiment$beast2_options$output_state_filename))
    expect_true(is_pff(experiment$beast2_options$beast2_working_dir))
    expect_true(is_pff(experiment$beast2_options$beast2_path))
  }
})
