test_that("matches article", {
  expect_equal(
    get_razzo_mutation_rate(),
    0.5 / get_razzo_crown_age()
  )
})
