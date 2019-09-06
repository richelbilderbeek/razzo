test_that("follows naming convention", {
  folder_name <- peregrine::get_pff_tempfile()
  twinning_params <- create_razzo_twinning_params(folder_name = folder_name)
  expect_equal(
    twinning_params$twin_tree_filename,
    file.path(folder_name, "mbd_twin.tree")
  )
  expect_equal(
    twinning_params$twin_alignment_filename,
    file.path(folder_name, "mbd_twin.fasta")
  )
  expect_equal(
    twinning_params$twin_evidence_filename,
    file.path(folder_name, "mbd_marg_lik_twin.csv")
  )

})

test_that("matches article", {
  folder_name <- peregrine::get_pff_tempfile()
  twinning_params <- create_razzo_twinning_params(folder_name = folder_name)
  expect_true(!beautier::is_one_na(pir_params$twinning_params))
  expect_equal(pir_params$twinning_params$twin_model, "birth_death")
  expect_equal(pir_params$twinning_params$method, "random_tree")
})
