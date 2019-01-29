context("convert_tree2brts")

test_that("use", {

  parameters <- open_parameters_file(get_path("parameters.csv"))
  max_sims <- 10 + (beastier::is_on_travis() * 10)
  for (seed in 1:max_sims) {
    mbd_sim <- create_mbd_tree(parameters)
    brts1 <- convert_tree2brts(mbd_sim$mbd_tree)
    brts2 <- ape::branching.times(mbd_sim$mbd_tree)

    expect_true(
      is.numeric(brts1)
    )
    expect_true(
      all(
        abs(brts1 - brts2) < 1e-6
      )
    )
  }
  set.seed(1)
  for (precision in 3:8) {
    mbd_sim <- create_mbd_tree(parameters)
    brts1 <- convert_tree2brts(mbd_sim$mbd_tree, precision = precision)
    brts2 <- ape::branching.times(mbd_sim$mbd_tree)

    expect_true(
      is.numeric(brts1)
    )
    expect_true(
      all(
        abs(brts1 - brts2) < 10 ^ (-precision)
      )
    )
  }

})
