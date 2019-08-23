test_that("use", {
  experiments <- pirouette::create_all_experiments()
  n_experiments_before <- length(experiments)
  n_cbs <- 0
  for (experiment in experiments) {
    cbs_tree_prior_name <- "coalescent_bayesian_skyline"
    testit::assert(cbs_tree_prior_name %in% beautier::get_tree_prior_names())
    if (experiment$inference_model$tree_prior$name == cbs_tree_prior_name) {
      n_cbs <- n_cbs + 1
    }
  }
  testit::assert(n_cbs > 0) # else the test has no use
  n_experiments_after <- n_experiments_before - n_cbs

  experiments <- remove_cbs_exps(experiments)

  expect_equal(length(experiments), n_experiments_after)
})
