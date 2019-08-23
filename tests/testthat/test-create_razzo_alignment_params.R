test_that("use", {
  folder_name <- peregrine::get_pff_tempfile()
  alignment_params <- create_razzo_alignment_params(folder_name)
  expect_equal(
    alignment_params$fasta_filename,
    file.path(folder_name, "mbd.fasta")
  )
})
