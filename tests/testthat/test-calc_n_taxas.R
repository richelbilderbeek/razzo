test_that("use", {

  # Issue 278, Issue #278: 'calc_n_taxas' is only a stub

  # Create a testing parameter set
   mbd_paramses <- create_test_mbd_paramses()

  # Per mbd_params, get the number of taxa of the true tree,
  # e.g. by simulating it in the same way razzo already does
  n_taxas <- calc_n_taxas(mbd_paramses)

  # n_taxa is a simple numerical list
  expect_true(is.numeric(n_taxas))

  # with as much elements as the razzo_paramses
  expect_equal(length(mbd_paramses), length(n_taxas))
})
