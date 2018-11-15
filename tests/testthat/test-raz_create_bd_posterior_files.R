context("raz_create_bd_posterior_files")

test_that("use", {

  if (!ribir::is_on_travis()) return()

  parameters_filename <- raz_create_tempfile("parameters.csv")
  bd_alignment_filename <- raz_create_tempfile("bd.fasta")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(bd_alignment_filename))

  bd_posterior_filenames <- raz_create_bd_posterior_files(
    parameters_filename = parameters_filename
  )


  expect_true(all(file.exists(bd_posterior_filenames)))

  bd_trees_filename <- grep(
    pattern = "bd\\.trees$",
    bd_posterior_filenames, perl = TRUE, value = TRUE
  )
  expect_true(length(bd_trees_filename) > 0)

  # TODO: Issue 100, #100
  # tracerer must be able to read the posterior trees
  if (1 == 2) {
    # Current error: Error in value[[3L]](cond) : invalid file
    expect_silent(
      tracerer::parse_beast_trees(bd_trees_filename)
    )
  }

  expect_true(length(grep(
    pattern = "bd\\.trees$",
    bd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
  expect_true(length(grep(
    pattern = "bd\\.log$",
    bd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
  log_filename <- grep(
    pattern = "bd\\.log$",
    bd_posterior_filenames, perl = TRUE, value = TRUE
  )

  estimates <- tracerer::parse_beast_log(log_filename)
  expect_equal("data.frame", class(estimates))
})
