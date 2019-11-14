test_that("matches article, generative", {
  folder_name <- peregrine::get_pff_tempfile()
  rng_seed <- 1234
  beast2_options <- create_razzo_beast2_options(
    model_type = "generative",
    rng_seed = rng_seed
  )
  expect_equal(
    beast2_options$rng_seed,
    rng_seed
  )
  expect_equal(
    beast2_options$overwrite,
    TRUE
  )
})

test_that("matches article, candidate", {
  folder_name <- peregrine::get_pff_tempfile()
  rng_seed <- 12345
  beast2_options <- create_razzo_beast2_options(
    model_type = "candidate",
    rng_seed = rng_seed
  )
  expect_equal(
    beast2_options$rng_seed,
    rng_seed
  )
  expect_equal(
    beast2_options$overwrite,
    TRUE
  )
})

test_that("follows naming conventions, generative", {
  folder_name <- peregrine::get_pff_tempfile()
  beast2_options <- create_razzo_beast2_options(
    model_type = "generative",
    folder_name = folder_name
  )
  expect_equal(
    beast2_options$input_filename,
    file.path(folder_name, "mbd_gen.xml")
  )
  expect_equal(
    beast2_options$output_state_filename,
    file.path(folder_name, "mbd_gen.xml.state")
  )
})

test_that("follows naming conventions, candidate", {
  folder_name <- peregrine::get_pff_tempfile()
  beast2_options <- create_razzo_beast2_options(
    model_type = "candidate",
    folder_name = folder_name
  )
  expect_equal(
    beast2_options$input_filename,
    file.path(folder_name, "mbd_best.xml")
  )
  expect_equal(
    beast2_options$output_state_filename,
    file.path(folder_name, "mbd_best.xml.state")
  )
})
