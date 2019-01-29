context("test-create_mbd_marg_lik_file")

test_that("must create file", {

  expect_true(1 + 1 == 2) # to get a non-empty test

  if (!beastier::is_on_travis()) return()
  if (rappdirs::app_dir()$os == "win") return()

  parameters_filename <- create_tempfile("parameters.csv")
  mbd_alignment_filename <- create_tempfile("mbd.fasta")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(mbd_alignment_filename))

  mbd_marg_lik_filename <- create_mbd_marg_lik_file(# nolint internal function
    parameters_filename = parameters_filename
  )
  expect_true(file.exists(mbd_marg_lik_filename))
  expect_true(length(grep(
    pattern = "mbd_marg_lik\\.csv$",
    mbd_marg_lik_filename, perl = TRUE, value = TRUE))
    > 0
  )
})

test_that("mbd and bd differ", {

  expect_true(1 + 1 == 2) # to get a non-empty test

  if (!beastier::is_on_travis()) return()
  if (rappdirs::app_dir()$os == "win") return()

  parameters_filename <- create_tempfile("parameters.csv")
  mbd_alignment_filename <- create_tempfile("mbd.fasta")
  bd_alignment_filename <- create_tempfile("bd.fasta")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(mbd_alignment_filename))
  testit::assert(file.exists(bd_alignment_filename))

  mbd_marg_lik_filename <- create_mbd_marg_lik_file(# nolint internal function
    parameters_filename = parameters_filename
  )
  bd_marg_lik_filename <- create_bd_marg_lik_file(# nolint internal function
    parameters_filename = parameters_filename
  )
  expect_true(file.exists(mbd_marg_lik_filename))
  expect_true(file.exists(bd_marg_lik_filename))

  mbd_marg_lik <- utils::read.csv(mbd_marg_lik_filename)
  bd_marg_lik <- utils::read.csv(bd_marg_lik_filename)
  expect_true(mbd_marg_lik$marg_log_lik != bd_marg_lik$marg_log_lik)
})
