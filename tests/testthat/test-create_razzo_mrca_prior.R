test_that("matches article", {
  mrca_prior <- create_razzo_mrca_prior()
  expect_true(mrca_prior$is_monophyletic)
  expect_true(!beautier::is_one_na(mrca_prior$mrca_distr))
  expect_equal(mrca_prior$mrca_distr$mean$value, get_razzo_crown_age())
  expect_equal(mrca_prior$mrca_distr$sigma$value, 0.0001)
})
